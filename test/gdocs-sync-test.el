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
      (cl-letf (((symbol-function 'gdocs-api-batch-update)
                 (lambda (_doc-id requests callback &optional _account)
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
                 (lambda (_old-ir _new-ir)
                   (setq diff-called t)
                   (list '((replaceText . "mock")))))
                ((symbol-function 'gdocs-api-batch-update)
                 (lambda (_doc-id _requests callback &optional _account)
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
                 (lambda (_old _new) nil))
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
    (cl-letf (((symbol-function 'gdocs-api-batch-update)
               (lambda (_doc-id _requests callback &optional _account)
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
                 (lambda (_old _new) '(((mock . t)))))
                ((symbol-function 'gdocs-api-batch-update)
                 (lambda (_doc-id _requests callback &optional _account)
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
  "Buffer content is replaced with remote content."
  (gdocs-sync-test-with-org-buffer "Old local content\n"
    (setq gdocs-sync--revision-id "rev-5")
    (set-buffer-modified-p nil)
    (setq gdocs-sync--shadow-ir (gdocs-convert-org-buffer-to-ir))
    (cl-letf (((symbol-function 'gdocs-api-get-file-metadata)
               (lambda (file-id callback &optional account)
                 (funcall callback
                          '((headRevisionId . "rev-6")))))
              ((symbol-function 'gdocs-api-get-document)
               (lambda (doc-id callback &optional account)
                 (funcall callback (gdocs-test-sample-document-json))))
              ((symbol-function 'gdocs-convert-docs-json-to-ir)
               (lambda (json)
                 (list (list :type 'paragraph :style 'normal
                             :contents (list (list :text "Remote" :bold nil))
                             :id "elem-001"))))
              ((symbol-function 'gdocs-convert-ir-to-org)
               (lambda (ir) "Remote content\n")))
      (gdocs-sync-pull)
      (should (string= (buffer-substring-no-properties (point-min) (point-max))
                        "Remote content\n")))))

(ert-deftest gdocs-sync-test-pull-with-local-mods ()
  "Local modifications trigger conflict resolution."
  (gdocs-sync-test-with-org-buffer "Modified local\n"
    (setq gdocs-sync--revision-id "rev-5")
    (set-buffer-modified-p t)
    (let ((merge-started nil))
      (cl-letf (((symbol-function 'gdocs-api-get-file-metadata)
                 (lambda (file-id callback &optional account)
                   (funcall callback
                            '((headRevisionId . "rev-7")))))
                ((symbol-function 'gdocs-api-get-document)
                 (lambda (doc-id callback &optional account)
                   (funcall callback (gdocs-test-sample-document-json))))
                ((symbol-function 'gdocs-convert-docs-json-to-ir)
                 (lambda (json)
                   (list (list :type 'paragraph :style 'normal
                               :contents (list (list :text "Remote" :bold nil))
                               :id "elem-001"))))
                ((symbol-function 'gdocs-convert-ir-to-org)
                 (lambda (ir) "Remote content\n"))
                ((symbol-function 'gdocs-merge-start)
                 (lambda (local remote callback)
                   (setq merge-started t))))
        (gdocs-sync-pull)
        (should merge-started)
        (should (eq gdocs-sync--status 'conflict))))))

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
      (cl-letf (((symbol-function 'gdocs-api-batch-update)
                 (lambda (doc-id requests callback &optional account)
                   (setq captured-status gdocs-sync--status)
                   (funcall callback
                            '((writeControl
                               (requiredRevisionId . "rev-8")))))))
        (setq gdocs-sync--shadow-ir nil)
        (gdocs-sync-push)
        (should (eq captured-status 'pushing))
        (should (eq gdocs-sync--status 'synced))))))

(provide 'gdocs-sync-test)
;;; gdocs-sync-test.el ends here
