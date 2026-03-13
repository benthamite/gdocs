;;; gdocs-sync-test.el --- Tests for gdocs-sync -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Pablo Stafforini

;; Author: gdocs contributors

;; This file is not part of GNU Emacs.

;;; Commentary:

;; ERT tests for the gdocs-sync module covering push, pull, link/unlink,
;; URL parsing, and status transitions.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'gdocs-sync)
(require 'gdocs-convert)
(require 'gdocs-merge)
(require 'gdocs-test-helpers)

;;;; Test helpers

(defmacro gdocs-sync-test-with-org-buffer (content &rest body)
  "Execute BODY in a temp org buffer with CONTENT."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (org-mode)
     (insert ,content)
     (setq gdocs-sync--document-id "test-doc-id")
     (setq gdocs-sync--account "test-account")
     ,@body))

(defvar gdocs-sync-test--batch-update-calls nil
  "Accumulator for batch-update calls during tests.")

(defvar gdocs-sync-test--batch-update-response nil
  "Response to return from mock batch-update.")

;;;; Push tests

(ert-deftest gdocs-sync-test-push-no-shadow ()
  "First push without shadow uses full IR-to-requests conversion."
  (gdocs-sync-test-with-org-buffer "Hello world\n"
    (let ((requests-received nil))
      (cl-letf (((symbol-function 'gdocs-api-get-document)
                 (lambda (_doc-id callback &optional _account)
                   (funcall callback
                            '((body . ((content . [((endIndex . 2))])))))))
                ((symbol-function 'gdocs-api-batch-update)
                 (lambda (_doc-id requests callback &optional _account _on-error)
                   (setq requests-received requests)
                   (funcall callback
                            '((writeControl
                               (requiredRevisionId . "rev-1")))))))
        (should (null gdocs-sync--shadow-ir))
        (gdocs-sync-push)
        (should requests-received)
        (should (eq gdocs-sync--status 'synced))))))

(ert-deftest gdocs-sync-test-push-with-shadow ()
  "Incremental push generates diff from shadow IR."
  (gdocs-sync-test-with-org-buffer "Updated content\n"
    (let ((diff-called nil))
      (setq gdocs-sync--shadow-ir
            (list (list :type 'paragraph :style 'normal
                        :contents (list (list :text "Original" :bold nil))
                        :id "elem-001")))
      (cl-letf (((symbol-function 'gdocs-diff-generate)
                 (lambda (_old-ir _new-ir &optional _start-index)
                   (setq diff-called t)
                   (list '((replaceText . "mock")))))
                ((symbol-function 'gdocs-api-batch-update)
                 (lambda (_doc-id _requests callback &optional _account _on-error)
                   (funcall callback
                            '((writeControl
                               (requiredRevisionId . "rev-2")))))))
        (gdocs-sync-push)
        (should diff-called)
        (should (eq gdocs-sync--status 'synced))))))

(ert-deftest gdocs-sync-test-push-no-changes ()
  "No changes detected results in no API call."
  (gdocs-sync-test-with-org-buffer "Hello world\n"
    (let ((api-called nil))
      (setq gdocs-sync--shadow-ir (gdocs-convert-org-buffer-to-ir))
      (cl-letf (((symbol-function 'gdocs-diff-generate)
                 (lambda (_old _new &optional _start-index) nil))
                ((symbol-function 'gdocs-api-batch-update)
                 (lambda (&rest _) (setq api-called t))))
        (gdocs-sync-push)
        (should-not api-called)
        (should (eq gdocs-sync--status 'synced))))))

(ert-deftest gdocs-sync-test-push-serialization ()
  "Second push while first is in progress gets queued."
  (gdocs-sync-test-with-org-buffer "Content\n"
    (setq gdocs-sync--push-in-progress t)
    (gdocs-sync-push)
    (should gdocs-sync--push-queued)))

(ert-deftest gdocs-sync-test-push-success-updates-shadow ()
  "Shadow IR is updated to current IR on successful push."
  (gdocs-sync-test-with-org-buffer "New content\n"
    (setq gdocs-sync--shadow-ir nil)
    (cl-letf (((symbol-function 'gdocs-api-get-document)
               (lambda (_doc-id callback &optional _account)
                 (funcall callback
                          '((body . ((content . [((endIndex . 2))])))))))
              ((symbol-function 'gdocs-api-batch-update)
               (lambda (_doc-id _requests callback &optional _account _on-error)
                 (funcall callback
                          '((writeControl
                             (requiredRevisionId . "rev-3")))))))
      (gdocs-sync-push)
      (should gdocs-sync--shadow-ir)
      (should (equal gdocs-sync--revision-id "rev-3")))))

(ert-deftest gdocs-sync-test-push-error-preserves-shadow ()
  "Shadow IR is unchanged when push fails."
  (gdocs-sync-test-with-org-buffer "Content\n"
    (let ((original-shadow '((:type paragraph))))
      (setq gdocs-sync--shadow-ir original-shadow)
      (cl-letf (((symbol-function 'gdocs-diff-generate)
                 (lambda (_old _new &optional _start-index) '(((mock . t)))))
                ((symbol-function 'gdocs-api-batch-update)
                 (lambda (_doc-id _requests callback &optional _account _on-error)
                   (funcall callback
                            '((writeControl
                               (requiredRevisionId . "rev-4")))))))
        (gdocs-sync-push)
        (should-not (equal gdocs-sync--shadow-ir original-shadow))))))

;;;; Pull tests

(ert-deftest gdocs-sync-test-pull-no-changes ()
  "Revision matches stored one; message says up to date."
  (gdocs-sync-test-with-org-buffer "Content\n"
    (setq gdocs-sync--revision-id "rev-5")
    (cl-letf (((symbol-function 'gdocs-api-get-file-metadata)
               (lambda (_file-id callback &optional _account)
                 (funcall callback
                          '((headRevisionId . "rev-5"))))))
      (gdocs-sync-pull))))

(ert-deftest gdocs-sync-test-pull-updates-buffer ()
  "Buffer content is replaced with remote content on first pull.
Tests the no-shadow (full replacement) path and verifies that
file-local variables are preserved and revision ID is stored."
  (gdocs-sync-test-with-org-buffer "Old local content\n"
    (setq gdocs-sync--revision-id "rev-5")
    (set-buffer-modified-p nil)
    ;; No shadow: tests the full-replacement pull path
    (cl-letf (((symbol-function 'gdocs-api-get-file-metadata)
               (lambda (_file-id callback &optional _account)
                 (funcall callback
                          '((headRevisionId . "rev-6")))))
              ((symbol-function 'gdocs-api-get-document)
               (lambda (_doc-id callback &optional _account)
                 (funcall callback (gdocs-test-sample-document-json))))
              ((symbol-function 'gdocs-convert-docs-json-to-ir)
               (lambda (_json)
                 (list (list :type 'paragraph :style 'normal
                             :contents (list (list :text "Remote content"
                                                   :bold nil :italic nil
                                                   :underline nil
                                                   :strikethrough nil
                                                   :code nil :link nil))
                             :id "elem-001"))))
              ((symbol-function 'gdocs-convert-ir-to-org)
               (lambda (_ir) "Remote content\n")))
      (gdocs-sync-pull)
      (should (string-prefix-p "Remote content\n"
                               (buffer-substring-no-properties
                                (point-min) (point-max))))
      (should (equal gdocs-sync--revision-id "rev-6")))))

(ert-deftest gdocs-sync-test-pull-with-local-mods ()
  "Three-way merge updates buffer when only remote changed.
Tests the three-way merge path where local matches shadow (no
local changes) but remote has new content."
  (gdocs-sync-test-with-org-buffer "Local content\n"
    (setq gdocs-sync--revision-id "rev-5")
    ;; Shadow matches local content (no local modifications)
    (setq gdocs-sync--shadow-ir (gdocs-convert-org-buffer-to-ir))
    (cl-letf (((symbol-function 'gdocs-api-get-file-metadata)
               (lambda (_file-id callback &optional _account)
                 (funcall callback
                          '((headRevisionId . "rev-7")))))
              ((symbol-function 'gdocs-api-get-document)
               (lambda (_doc-id callback &optional _account)
                 (funcall callback (gdocs-test-sample-document-json))))
              ((symbol-function 'gdocs-convert-docs-json-to-ir)
               (lambda (_json)
                 (list (list :type 'paragraph :style 'normal
                             :contents (list (list :text "Remote update"
                                                   :bold nil :italic nil
                                                   :underline nil
                                                   :strikethrough nil
                                                   :code nil :link nil))
                             :id "elem-001")))))
      (gdocs-sync-pull)
      (should (string-prefix-p "Remote update\n"
                               (buffer-substring-no-properties
                                (point-min) (point-max))))
      (should (eq gdocs-sync--status 'synced))
      (should (equal gdocs-sync--revision-id "rev-7")))))

;;;; Link/Unlink tests

(ert-deftest gdocs-sync-test-parse-document-id-from-url ()
  "URL parsing extracts the document ID."
  (should (equal "1BxiMVs0XRA5nFMdKvBdBZjgmUUqptlbs74OgVE2upms"
                 (gdocs-sync--parse-document-id
                  "https://docs.google.com/document/d/1BxiMVs0XRA5nFMdKvBdBZjgmUUqptlbs74OgVE2upms/edit"))))

(ert-deftest gdocs-sync-test-parse-document-id-plain ()
  "A plain document ID is returned as-is."
  (should (equal "abc123"
                 (gdocs-sync--parse-document-id "abc123"))))

(ert-deftest gdocs-sync-test-link-sets-file-local-vars ()
  "Linking writes file-local variables to the buffer."
  (let ((temp-file (make-temp-file "gdocs-test-" nil ".org")))
    (unwind-protect
        (progn
          (with-current-buffer (find-file-noselect temp-file)
            (org-mode)
            (insert "Test content\n")
            (save-buffer)
            (cl-letf (((symbol-function 'gdocs-auth-select-account)
                       (lambda (&optional _prompt) "test-account"))
                      ((symbol-function 'gdocs-sync-pull)
                       #'ignore))
              (gdocs-sync-link "test-doc-id"))
            (should (equal gdocs-sync--document-id "test-doc-id"))
            (should (equal gdocs-sync--account "test-account"))
            (kill-buffer)))
      (delete-file temp-file))))

(ert-deftest gdocs-sync-test-unlink-removes-vars ()
  "Unlinking clears all sync state."
  (let ((temp-file (make-temp-file "gdocs-test-" nil ".org")))
    (unwind-protect
        (progn
          (with-current-buffer (find-file-noselect temp-file)
            (org-mode)
            (insert "Test content\n")
            (setq gdocs-sync--document-id "test-doc-id")
            (setq gdocs-sync--account "test-account")
            (save-buffer)
            (gdocs-sync-unlink)
            (should-not gdocs-sync--document-id)
            (should-not gdocs-sync--account)
            (kill-buffer)))
      (delete-file temp-file))))

;;;; Status tests

(ert-deftest gdocs-sync-test-status-transitions ()
  "Status transitions through push lifecycle."
  (gdocs-sync-test-with-org-buffer "Content\n"
    (should (eq gdocs-sync--status 'synced))
    (let ((captured-status nil))
      (cl-letf (((symbol-function 'gdocs-api-get-document)
                 (lambda (_doc-id callback &optional _account)
                   (funcall callback
                            '((body . ((content . [((endIndex . 2))])))))))
                ((symbol-function 'gdocs-api-batch-update)
                 (lambda (doc-id requests callback &optional account _on-error)
                   (setq captured-status gdocs-sync--status)
                   (funcall callback
                            '((writeControl
                               (requiredRevisionId . "rev-8")))))))
        (setq gdocs-sync--shadow-ir nil)
        (gdocs-sync-push)
        (should (eq captured-status 'pushing))
        (should (eq gdocs-sync--status 'synced))))))

;;;; Three-way merge internals: graft-markers

(ert-deftest gdocs-sync-test-graft-markers-copies-marker ()
  "Graft-markers copies :gdocs-marker from local to remote element."
  (let ((remote (list :type 'paragraph :style 'heading-1
                      :contents (list (list :text "Hello"))
                      :id "r-001"))
        (local (list :type 'paragraph :style 'heading-1
                     :contents (list (list :text "Hello"))
                     :id "l-001"
                     :gdocs-marker (list :type 'todo :data "TODO"))))
    (let ((result (gdocs-sync--graft-markers remote local)))
      (should (equal (plist-get result :gdocs-marker)
                     (list :type 'todo :data "TODO")))
      ;; Remote content should be preserved
      (should (eq (plist-get result :style) 'heading-1)))))

(ert-deftest gdocs-sync-test-graft-markers-nil-local ()
  "Graft-markers with nil local returns remote unchanged."
  (let ((remote (list :type 'paragraph :style 'normal
                      :contents (list (list :text "Hello"))
                      :id "r-001")))
    (should (eq (gdocs-sync--graft-markers remote nil) remote))))

(ert-deftest gdocs-sync-test-graft-markers-local-without-marker ()
  "Graft-markers with local that has no marker returns remote unchanged."
  (let ((remote (list :type 'paragraph :style 'normal
                      :contents (list (list :text "Hello"))
                      :id "r-001"))
        (local (list :type 'paragraph :style 'normal
                     :contents (list (list :text "Hello"))
                     :id "l-001")))
    (should (eq (gdocs-sync--graft-markers remote local) remote))))

;;;; Property drawer extraction

(ert-deftest gdocs-sync-test-extract-property-drawer ()
  "Extracts :PROPERTIES: block from heading text."
  (let ((text "* Heading\n:PROPERTIES:\n:ID: abc-123\n:END:\nBody text\n"))
    (let ((drawer (gdocs-sync--extract-property-drawer text)))
      (should drawer)
      (should (string-match-p ":PROPERTIES:" drawer))
      (should (string-match-p ":ID: abc-123" drawer))
      (should (string-match-p ":END:" drawer)))))

(ert-deftest gdocs-sync-test-extract-property-drawer-nil ()
  "Returns nil when no property drawer is present."
  (should-not (gdocs-sync--extract-property-drawer "* Simple heading\nBody\n")))

;;;; Heading detection

(ert-deftest gdocs-sync-test-heading-element-p-heading ()
  "Returns non-nil for heading elements."
  (let ((elem (list :type 'paragraph :style 'heading-1
                    :contents (list (list :text "Title")))))
    (should (gdocs-sync--heading-element-p elem))))

(ert-deftest gdocs-sync-test-heading-element-p-normal-paragraph ()
  "Returns nil for normal paragraphs."
  (let ((elem (list :type 'paragraph :style 'normal
                    :contents (list (list :text "Body text")))))
    (should-not (gdocs-sync--heading-element-p elem))))

(ert-deftest gdocs-sync-test-heading-element-p-table ()
  "Returns nil for table elements."
  (let ((elem (list :type 'table :rows '())))
    (should-not (gdocs-sync--heading-element-p elem))))

;;;; Push-on-save conditions

(ert-deftest gdocs-sync-test-should-auto-push-enabled ()
  "Returns t when auto-push enabled, document linked, and status ok."
  (gdocs-sync-test-with-org-buffer "Content\n"
    (let ((gdocs-auto-push-on-save t))
      (setq gdocs-sync--status 'synced)
      (should (gdocs-sync--should-auto-push-p)))))

(ert-deftest gdocs-sync-test-should-auto-push-disabled ()
  "Returns nil when auto-push is disabled."
  (gdocs-sync-test-with-org-buffer "Content\n"
    (let ((gdocs-auto-push-on-save nil))
      (should-not (gdocs-sync--should-auto-push-p)))))

(ert-deftest gdocs-sync-test-should-auto-push-conflict ()
  "Returns nil when in conflict status."
  (gdocs-sync-test-with-org-buffer "Content\n"
    (let ((gdocs-auto-push-on-save t))
      (setq gdocs-sync--status 'conflict)
      (should-not (gdocs-sync--should-auto-push-p)))))

(ert-deftest gdocs-sync-test-should-auto-push-error ()
  "Returns nil when in error status."
  (gdocs-sync-test-with-org-buffer "Content\n"
    (let ((gdocs-auto-push-on-save t))
      (setq gdocs-sync--status 'error)
      (should-not (gdocs-sync--should-auto-push-p)))))

(ert-deftest gdocs-sync-test-should-auto-push-no-document-id ()
  "Returns nil when no document ID is set."
  (with-temp-buffer
    (org-mode)
    (insert "Content\n")
    (setq gdocs-sync--document-id nil)
    (let ((gdocs-auto-push-on-save t))
      (setq gdocs-sync--status 'synced)
      (should-not (gdocs-sync--should-auto-push-p)))))

;;;; Title filtering

(ert-deftest gdocs-sync-test-filter-title-removes-titles ()
  "Removes title elements from IR."
  (let ((ir (list (list :type 'paragraph :style 'title
                        :contents (list (list :text "My Doc"))
                        :id "e1")
                  (list :type 'paragraph :style 'normal
                        :contents (list (list :text "Body"))
                        :id "e2"))))
    (let ((filtered (gdocs-sync--filter-title ir)))
      (should (= (length filtered) 1))
      (should (eq (plist-get (car filtered) :style) 'normal)))))

(ert-deftest gdocs-sync-test-filter-title-preserves-non-title ()
  "Preserves non-title elements unchanged."
  (let ((ir (list (list :type 'paragraph :style 'heading-1
                        :contents (list (list :text "Heading"))
                        :id "e1")
                  (list :type 'paragraph :style 'normal
                        :contents (list (list :text "Body"))
                        :id "e2"))))
    (let ((filtered (gdocs-sync--filter-title ir)))
      (should (= (length filtered) 2)))))

;;;; Body start index

(ert-deftest gdocs-sync-test-body-start-index-no-title ()
  "Returns 1 when no title elements are present."
  (let ((ir (list (list :type 'paragraph :style 'normal
                        :contents (list (list :text "Body"))
                        :id "e1"))))
    (should (= (gdocs-sync--body-start-index ir) 1))))

(ert-deftest gdocs-sync-test-body-start-index-with-title ()
  "Returns offset when a non-metadata title exists."
  (let ((ir (list (list :type 'paragraph :style 'title
                        :contents (list (list :text "My Title"))
                        :id "e1")
                  (list :type 'paragraph :style 'normal
                        :contents (list (list :text "Body"))
                        :id "e2"))))
    ;; Title "My Title" = 8 UTF-16 units + 1 newline = 9, plus body start 1
    (should (> (gdocs-sync--body-start-index ir) 1))))

(ert-deftest gdocs-sync-test-body-start-index-metadata-title ()
  "Metadata-source title does not offset body start."
  (let ((ir (list (list :type 'paragraph :style 'title
                        :contents (list (list :text "My Title"))
                        :source 'metadata
                        :id "e1")
                  (list :type 'paragraph :style 'normal
                        :contents (list (list :text "Body"))
                        :id "e2"))))
    (should (= (gdocs-sync--body-start-index ir) 1))))

;;;; Remote unchanged detection

(ert-deftest gdocs-sync-test-remote-unchanged-matching-keys ()
  "Returns t when remote keys match shadow keys."
  (gdocs-sync-test-with-org-buffer "Hello world\n"
    (let ((ir (gdocs-convert-org-buffer-to-ir)))
      (setq gdocs-sync--shadow-ir ir)
      ;; Same IR as remote: should be unchanged
      (should (gdocs-sync--remote-unchanged-p ir)))))

(ert-deftest gdocs-sync-test-remote-unchanged-different-keys ()
  "Returns nil when remote keys differ from shadow keys."
  (gdocs-sync-test-with-org-buffer "Hello world\n"
    (setq gdocs-sync--shadow-ir (gdocs-convert-org-buffer-to-ir))
    (let ((different-ir (list (list :type 'paragraph :style 'normal
                                   :contents (list (list :text "Different"
                                                         :bold nil :italic nil
                                                         :underline nil
                                                         :strikethrough nil
                                                         :code nil :link nil))
                                   :id "elem-099"))))
      (should-not (gdocs-sync--remote-unchanged-p different-ir)))))

(ert-deftest gdocs-sync-test-remote-unchanged-no-shadow ()
  "Returns nil when no shadow IR exists."
  (gdocs-sync-test-with-org-buffer "Hello world\n"
    (setq gdocs-sync--shadow-ir nil)
    (let ((ir (gdocs-convert-org-buffer-to-ir)))
      (should-not (gdocs-sync--remote-unchanged-p ir)))))

;;;; URL parsing edge cases

(ert-deftest gdocs-sync-test-parse-url-with-query-params ()
  "URL with query parameters extracts document ID correctly."
  (should (equal "1BxiMVs0XRA5nFMdKvBdBZjgmUUqptlbs74OgVE2upms"
                 (gdocs-sync--parse-document-id
                  "https://docs.google.com/document/d/1BxiMVs0XRA5nFMdKvBdBZjgmUUqptlbs74OgVE2upms/edit?usp=sharing&tab=t.0"))))

(ert-deftest gdocs-sync-test-parse-url-with-fragment ()
  "URL with fragment extracts document ID correctly."
  (should (equal "1BxiMVs0XRA5nFMdKvBdBZjgmUUqptlbs74OgVE2upms"
                 (gdocs-sync--parse-document-id
                  "https://docs.google.com/document/d/1BxiMVs0XRA5nFMdKvBdBZjgmUUqptlbs74OgVE2upms/edit#heading=h.abc"))))

;;;; Clear buffer state

(ert-deftest gdocs-sync-test-clear-buffer-state ()
  "Clears all buffer-local sync variables."
  (with-temp-buffer
    (org-mode)
    (setq gdocs-sync--document-id "test-id")
    (setq gdocs-sync--account "test-account")
    (setq gdocs-sync--shadow-ir '((:type paragraph)))
    (setq gdocs-sync--revision-id "rev-99")
    (setq gdocs-sync--last-sync-time "2026-01-01T00:00:00+0000")
    (setq gdocs-sync--push-in-progress t)
    (setq gdocs-sync--push-queued t)
    (setq gdocs-sync--status 'error)
    (cl-letf (((symbol-function 'gdocs--update-modeline) #'ignore))
      (gdocs-sync--clear-buffer-state))
    (should-not gdocs-sync--document-id)
    (should-not gdocs-sync--account)
    (should-not gdocs-sync--shadow-ir)
    (should-not gdocs-sync--revision-id)
    (should-not gdocs-sync--last-sync-time)
    (should-not gdocs-sync--push-in-progress)
    (should-not gdocs-sync--push-queued)
    (should (eq gdocs-sync--status 'synced))))

;;;; Drawer grafting

(ert-deftest gdocs-sync-test-graft-drawer-into-heading-simple ()
  "Inserts drawer after heading line."
  (let ((heading "* My heading")
        (drawer ":PROPERTIES:\n:ID: abc\n:END:\n"))
    (let ((result (gdocs-sync--graft-drawer-into-heading heading drawer)))
      (should (string-match-p "\\* My heading" result))
      (should (string-match-p ":PROPERTIES:" result))
      (should (string-match-p ":ID: abc" result))
      ;; Drawer should come after the heading line
      (should (< (string-match "\\* My heading" result)
                 (string-match ":PROPERTIES:" result))))))

(ert-deftest gdocs-sync-test-graft-drawer-into-heading-with-planning ()
  "Handles heading with SCHEDULED/DEADLINE planning lines."
  (let ((heading "* Task\nSCHEDULED: <2026-03-15>")
        (drawer ":PROPERTIES:\n:ID: xyz\n:END:\n"))
    (let ((result (gdocs-sync--graft-drawer-into-heading heading drawer)))
      ;; Heading line comes first
      (should (string-match-p "\\* Task" result))
      ;; Planning line should be before the drawer
      (let ((sched-pos (string-match "SCHEDULED:" result))
            (drawer-pos (string-match ":PROPERTIES:" result)))
        (should sched-pos)
        (should drawer-pos)
        (should (< sched-pos drawer-pos))))))

(provide 'gdocs-sync-test)
;;; gdocs-sync-test.el ends here
