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
  "First push without shadow uses incremental diff against remote.
Previously this used full replacement which destroyed Google Docs
comments; now it always diffs against the fetched remote document."
  (gdocs-sync-test-with-org-buffer "Hello world\n"
    (let ((diff-called nil))
      (cl-letf (((symbol-function 'gdocs-api-get-document)
                 (lambda (_doc-id callback &optional _account _on-error)
                   (funcall callback
                            '((title . "Test")
                              (body . ((content
                                        . [((startIndex . 0)
                                            (endIndex . 1)
                                            (sectionBreak . t))
                                           ((startIndex . 1)
                                            (endIndex . 2)
                                            (paragraph
                                             (elements . [((textRun
                                                            (content . "\n")
                                                            (textStyle)))])
                                             (paragraphStyle
                                              (namedStyleType . "NORMAL_TEXT"))))])))))))
                ((symbol-function 'gdocs-diff-generate)
                 (lambda (_old-ir _new-ir &optional _start-index)
                   (setq diff-called t)
                   (list '((insertText . "mock")))))
                ((symbol-function 'gdocs-api-batch-update)
                 (lambda (_doc-id _requests callback &optional _account _on-error)
                   (funcall callback
                            '((writeControl
                               (requiredRevisionId . "rev-1")))))))
        (should (null gdocs-sync--shadow-ir))
        (gdocs-sync-push)
        (should diff-called)
        (should (eq gdocs-sync--status 'synced))))))

(ert-deftest gdocs-sync-test-push-with-shadow ()
  "Incremental push fetches document and generates diff."
  (gdocs-sync-test-with-org-buffer "Updated content\n"
    (let ((diff-called nil))
      (setq gdocs-sync--shadow-ir
            (list (list :type 'paragraph :style 'normal
                        :contents (list (list :text "Original" :bold nil))
                        :id "elem-001")))
      (cl-letf (((symbol-function 'gdocs-api-get-document)
                 (lambda (_doc-id callback &optional _account _on-error)
                   (funcall callback
                            '((title . "Test")
                              (body . ((content
                                        . [((startIndex . 0)
                                            (endIndex . 1)
                                            (sectionBreak . t))
                                           ((startIndex . 1)
                                            (endIndex . 10)
                                            (paragraph
                                             (elements . [((textRun
                                                            (content . "Original\n")
                                                            (textStyle)))])
                                             (paragraphStyle
                                              (namedStyleType . "NORMAL_TEXT"))))])))))))
                ((symbol-function 'gdocs-diff-generate)
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
      (cl-letf (((symbol-function 'gdocs-api-get-document)
                 (lambda (_doc-id callback &optional _account _on-error)
                   (funcall callback
                            '((title . "Test")
                              (body . ((content
                                        . [((startIndex . 0)
                                            (endIndex . 1)
                                            (sectionBreak . t))
                                           ((startIndex . 1)
                                            (endIndex . 13)
                                            (paragraph
                                             (elements . [((textRun
                                                            (content . "Hello world\n")
                                                            (textStyle)))])
                                             (paragraphStyle
                                              (namedStyleType . "NORMAL_TEXT"))))])))))))
                ((symbol-function 'gdocs-diff-generate)
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
               (lambda (_doc-id callback &optional _account _on-error)
                 (funcall callback
                          '((title . "Test")
                            (body . ((content
                                      . [((startIndex . 0)
                                          (endIndex . 1)
                                          (sectionBreak . t))
                                         ((startIndex . 1)
                                          (endIndex . 2)
                                          (paragraph
                                           (elements . [((textRun
                                                          (content . "\n")
                                                          (textStyle)))])
                                           (paragraphStyle
                                            (namedStyleType . "NORMAL_TEXT"))))])))))))
              ((symbol-function 'gdocs-diff-generate)
               (lambda (_old-ir _new-ir &optional _start-index)
                 (list '((insertText . "mock")))))
              ((symbol-function 'gdocs-api-batch-update)
               (lambda (_doc-id _requests callback &optional _account _on-error)
                 (funcall callback
                          '((writeControl
                             (requiredRevisionId . "rev-3")))))))
      (gdocs-sync-push)
      (should gdocs-sync--shadow-ir)
      (should (equal gdocs-sync--revision-id "rev-3")))))

(ert-deftest gdocs-sync-test-push-error-preserves-shadow ()
  "Shadow IR is updated on successful push."
  (gdocs-sync-test-with-org-buffer "Content\n"
    (let ((original-shadow '((:type paragraph))))
      (setq gdocs-sync--shadow-ir original-shadow)
      (cl-letf (((symbol-function 'gdocs-api-get-document)
                 (lambda (_doc-id callback &optional _account _on-error)
                   (funcall callback
                            '((title . "Test")
                              (body . ((content
                                        . [((startIndex . 0)
                                            (endIndex . 1)
                                            (sectionBreak . t))
                                           ((startIndex . 1)
                                            (endIndex . 9)
                                            (paragraph
                                             (elements . [((textRun
                                                            (content . "Content\n")
                                                            (textStyle)))])
                                             (paragraphStyle
                                              (namedStyleType . "NORMAL_TEXT"))))])))))))
                ((symbol-function 'gdocs-diff-generate)
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
properties are preserved and revision ID is stored."
  (gdocs-sync-test-with-org-buffer "Old local content\n"
    (setq gdocs-sync--revision-id "rev-5")
    (set-buffer-modified-p nil)
    ;; No shadow: tests the full-replacement pull path
    (cl-letf (((symbol-function 'gdocs-api-get-file-metadata)
               (lambda (_file-id callback &optional _account)
                 (funcall callback
                          '((headRevisionId . "rev-6")))))
              ((symbol-function 'gdocs-api-get-document)
               (lambda (_doc-id callback &optional _account _on-error)
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
      (should (string-match-p "Remote content"
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
               (lambda (_doc-id callback &optional _account _on-error)
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
      (should (string-match-p "Remote update"
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

(ert-deftest gdocs-sync-test-link-sets-properties ()
  "Linking writes properties to the buffer."
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
                 (lambda (_doc-id callback &optional _account _on-error)
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
                     :gdocs-marker (list (list :type 'todo :data "TODO")))))
    (let ((result (gdocs-sync--graft-markers remote local)))
      (should (equal (plist-get result :gdocs-marker)
                     (list (list :type 'todo :data "TODO"))))
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

;;;; Folder URL parsing

(ert-deftest gdocs-sync-test-parse-folder-id-from-url ()
  "Extracts folder ID from a Google Drive folder URL."
  (should (equal "1AbCdEfGhIjKlMnOpQrStUvWxYz"
                 (gdocs-sync--parse-folder-id
                  "https://drive.google.com/drive/folders/1AbCdEfGhIjKlMnOpQrStUvWxYz"))))

(ert-deftest gdocs-sync-test-parse-folder-id-plain ()
  "A plain folder ID passes through unchanged."
  (should (equal "1AbCdEfGhIjKlMnOpQrStUvWxYz"
                 (gdocs-sync--parse-folder-id "1AbCdEfGhIjKlMnOpQrStUvWxYz"))))

(ert-deftest gdocs-sync-test-parse-folder-id-with-user-segment ()
  "URL with /u/0/ user segment extracts folder ID correctly."
  (should (equal "1AbCdEfGhIjKlMnOpQrStUvWxYz"
                 (gdocs-sync--parse-folder-id
                  "https://drive.google.com/drive/u/0/folders/1AbCdEfGhIjKlMnOpQrStUvWxYz"))))

;;;; Three-way merge

(ert-deftest gdocs-sync-test-three-way-merge-remote-only-change ()
  "Remote modifies one element, local unchanged -> merged uses remote."
  (gdocs-sync-test-with-org-buffer "Hello\n"
    (let* ((shadow-ir (gdocs-convert-org-buffer-to-ir))
           (remote-ir
            (list (list :type 'paragraph :style 'normal
                        :contents (list (list :text "Goodbye"
                                              :bold nil :italic nil
                                              :underline nil
                                              :strikethrough nil
                                              :code nil :link nil))
                        :id "elem-001"))))
      (setq gdocs-sync--shadow-ir shadow-ir)
      (let ((result (gdocs-sync--three-way-merge remote-ir)))
        (should-not (plist-get result :has-conflicts))
        (should (string-match-p "Goodbye" (plist-get result :merged-org)))))))

(ert-deftest gdocs-sync-test-three-way-merge-local-only-change ()
  "Remote unchanged, local adds a property drawer -> preserves local text.
The local modification (a property drawer) does not alter element
keys, so the LCS between shadow and local still matches.  The
merge should preserve the local org text verbatim, including the
drawer that Google Docs cannot represent."
  (gdocs-sync-test-with-org-buffer "* Heading\n\nBody text\n"
    (let* ((shadow-ir (gdocs-convert-org-buffer-to-ir))
           ;; Remote is the same as shadow (no remote changes)
           (remote-ir (gdocs-convert-org-buffer-to-ir)))
      (setq gdocs-sync--shadow-ir shadow-ir)
      ;; Now add a property drawer locally -- this does not change
      ;; the element key because drawers are org-only metadata.
      (goto-char (point-min))
      (forward-line 1)
      (insert ":PROPERTIES:\n:ID: local-id\n:END:\n")
      (let ((result (gdocs-sync--three-way-merge remote-ir)))
        (should-not (plist-get result :has-conflicts))
        (let ((merged (plist-get result :merged-org)))
          (should (string-match-p ":PROPERTIES:" merged))
          (should (string-match-p ":ID: local-id" merged)))))))

(ert-deftest gdocs-sync-test-three-way-merge-both-modify-same-element ()
  "Both sides modify the same element -> conflict detected.
The diff engine's adjacent delete+insert collapsing recognizes
the positional correspondence between the shadow element and
local modification, allowing the merge to detect the conflict."
  (gdocs-sync-test-with-org-buffer "Anchor paragraph\n\nBase text\n"
    (let* ((shadow-ir (gdocs-convert-org-buffer-to-ir))
           ;; Remote changes the second paragraph
           (remote-ir
            (list (car shadow-ir)
                  (list :type 'paragraph :style 'normal
                        :contents (list (list :text "Remote text"
                                              :bold nil :italic nil
                                              :underline nil
                                              :strikethrough nil
                                              :code nil :link nil))
                        :id "elem-remote"))))
      (setq gdocs-sync--shadow-ir shadow-ir)
      ;; Local also changes the second paragraph
      (erase-buffer)
      (insert "Anchor paragraph\n\nLocal text\n")
      (let ((result (gdocs-sync--three-way-merge remote-ir)))
        (should (plist-get result :has-conflicts))
        (let ((merged (plist-get result :merged-org)))
          (should (string-match-p "Anchor paragraph" merged))
          (should (string-match-p "<<<< LOCAL" merged))
          (should (string-match-p "Local text" merged))
          (should (string-match-p ">>>> REMOTE" merged))
          (should (string-match-p "Remote text" merged)))))))

(ert-deftest gdocs-sync-test-three-way-merge-remote-insert ()
  "Remote adds a new element not in shadow -> appears in merged output."
  (gdocs-sync-test-with-org-buffer "First\n"
    (let* ((shadow-ir (gdocs-convert-org-buffer-to-ir))
           ;; Remote has the original element plus a new one
           (remote-ir
            (append
             (gdocs-convert-org-buffer-to-ir)
             (list (list :type 'paragraph :style 'normal
                         :contents (list (list :text "Inserted"
                                               :bold nil :italic nil
                                               :underline nil
                                               :strikethrough nil
                                               :code nil :link nil))
                         :id "elem-new")))))
      (setq gdocs-sync--shadow-ir shadow-ir)
      (let ((result (gdocs-sync--three-way-merge remote-ir)))
        (should-not (plist-get result :has-conflicts))
        (let ((merged (plist-get result :merged-org)))
          (should (string-match-p "First" merged))
          (should (string-match-p "Inserted" merged)))))))

(ert-deftest gdocs-sync-test-three-way-merge-remote-delete-local-unchanged ()
  "Remote deletes element, local unchanged -> element removed."
  (gdocs-sync-test-with-org-buffer "Alpha\n\nBravo\n"
    (let* ((shadow-ir (gdocs-convert-org-buffer-to-ir))
           ;; Remote keeps only the first paragraph (deletes Bravo)
           (remote-ir
            (list (car shadow-ir))))
      (setq gdocs-sync--shadow-ir shadow-ir)
      (let ((result (gdocs-sync--three-way-merge remote-ir)))
        (should-not (plist-get result :has-conflicts))
        (let ((merged (plist-get result :merged-org)))
          (should (string-match-p "Alpha" merged))
          (should-not (string-match-p "Bravo" merged)))))))

(ert-deftest gdocs-sync-test-three-way-merge-remote-delete-local-modified ()
  "Remote deletes an element that local modified -> conflict.
The diff engine recognizes the local modification via adjacent
delete+insert collapsing, so the merge detects that remote
deleted something local still cares about."
  (gdocs-sync-test-with-org-buffer "Anchor paragraph\n\nBase text\n"
    (let* ((shadow-ir (gdocs-convert-org-buffer-to-ir))
           ;; Remote deletes the second paragraph, keeps only the anchor
           (remote-ir (list (car shadow-ir))))
      (setq gdocs-sync--shadow-ir shadow-ir)
      ;; Local modifies the second paragraph
      (erase-buffer)
      (insert "Anchor paragraph\n\nLocal modified text\n")
      (let ((result (gdocs-sync--three-way-merge remote-ir)))
        (should (plist-get result :has-conflicts))
        (let ((merged (plist-get result :merged-org)))
          (should (string-match-p "Anchor paragraph" merged))
          (should (string-match-p "Local modified text" merged)))))))

(ert-deftest gdocs-sync-test-three-way-merge-local-modify-remote-keeps ()
  "Local modifies content, remote keeps it unchanged -> local preserved.
When local changes an element's text (producing a different key)
and remote keeps the original, the merge should preserve the
local version since remote made no changes."
  (gdocs-sync-test-with-org-buffer "Anchor paragraph\n\nOriginal text\n"
    (let* ((shadow-ir (gdocs-convert-org-buffer-to-ir))
           ;; Remote is unchanged from shadow
           (remote-ir (gdocs-convert-org-buffer-to-ir)))
      (setq gdocs-sync--shadow-ir shadow-ir)
      ;; Local modifies the second paragraph
      (erase-buffer)
      (insert "Anchor paragraph\n\nLocal edit here\n")
      (let ((result (gdocs-sync--three-way-merge remote-ir)))
        (should-not (plist-get result :has-conflicts))
        (let ((merged (plist-get result :merged-org)))
          (should (string-match-p "Anchor paragraph" merged))
          (should (string-match-p "Local edit here" merged))
          (should-not (string-match-p "Original text" merged)))))))

;;;; Push-on-save: pushing status

(ert-deftest gdocs-sync-test-should-auto-push-when-pushing ()
  "Auto-push returns t when status is `pushing'.
The `pushing' status is NOT blocked by `gdocs-sync--should-auto-push-p';
serialization is handled separately by `gdocs-sync--serialize-push'."
  (gdocs-sync-test-with-org-buffer "Content\n"
    (let ((gdocs-auto-push-on-save t))
      (setq gdocs-sync--status 'pushing)
      ;; `pushing' is not in the exclusion list (conflict error),
      ;; so auto-push returns t -- serialization guards against double-push.
      (should (gdocs-sync--should-auto-push-p)))))

;;;; Extract title

(ert-deftest gdocs-sync-test-extract-title-present ()
  "Returns title text from IR with a title element."
  (let ((ir (list (list :type 'paragraph :style 'title
                        :contents (list (list :text "My Document"))
                        :source 'metadata
                        :id "e-title")
                  (list :type 'paragraph :style 'normal
                        :contents (list (list :text "Body"))
                        :id "e-body"))))
    (should (equal "My Document" (gdocs-sync--extract-title ir)))))

(ert-deftest gdocs-sync-test-extract-title-absent ()
  "Returns nil when no title element exists."
  (let ((ir (list (list :type 'paragraph :style 'normal
                        :contents (list (list :text "Body"))
                        :id "e-body"))))
    (should-not (gdocs-sync--extract-title ir))))

(provide 'gdocs-sync-test)
;;; gdocs-sync-test.el ends here
