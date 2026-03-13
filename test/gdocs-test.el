;;; gdocs-test.el --- Tests for gdocs -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Pablo Stafforini

;; Author: gdocs contributors

;; This file is not part of GNU Emacs.

;;; Commentary:

;; ERT tests for the main gdocs module covering filename sanitization,
;; URL construction, buffer title extraction, auto-activation,
;; keymap bindings, file-local variable safety, mode enable/disable,
;; and user commands.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'gdocs)

;;;; Filename sanitization

(ert-deftest gdocs-test-sanitize-simple-ascii ()
  "Simple ASCII names pass through unchanged."
  (should (equal "hello" (gdocs--sanitize-filename "hello"))))

(ert-deftest gdocs-test-sanitize-spaces-to-hyphens ()
  "Spaces are replaced with hyphens."
  (should (equal "hello-world" (gdocs--sanitize-filename "hello world"))))

(ert-deftest gdocs-test-sanitize-non-ascii-replaced ()
  "Non-ASCII characters are replaced with hyphens."
  (should (equal "caf-" (gdocs--sanitize-filename "caf\u00e9")))
  (should (equal "-" (gdocs--sanitize-filename "\u00fc"))))

(ert-deftest gdocs-test-sanitize-unsafe-characters ()
  "Filesystem-unsafe characters (/ \\ : ? * < > | \") are replaced."
  (should (equal "a-b" (gdocs--sanitize-filename "a/b")))
  (should (equal "a-b" (gdocs--sanitize-filename "a\\b")))
  (should (equal "a-b" (gdocs--sanitize-filename "a:b")))
  (should (equal "a-b" (gdocs--sanitize-filename "a?b")))
  (should (equal "a-b" (gdocs--sanitize-filename "a*b")))
  (should (equal "a-b" (gdocs--sanitize-filename "a<b")))
  (should (equal "a-b" (gdocs--sanitize-filename "a>b")))
  (should (equal "a-b" (gdocs--sanitize-filename "a|b")))
  (should (equal "a-b" (gdocs--sanitize-filename "a\"b"))))

(ert-deftest gdocs-test-sanitize-consecutive-hyphens-collapsed ()
  "Consecutive hyphens are collapsed into a single hyphen."
  (should (equal "a-b" (gdocs--sanitize-filename "a---b")))
  (should (equal "a-b" (gdocs--sanitize-filename "a / b"))))

(ert-deftest gdocs-test-sanitize-dots-preserved ()
  "Dots are preserved in filenames."
  (should (equal "file.backup" (gdocs--sanitize-filename "file.backup"))))

(ert-deftest gdocs-test-sanitize-underscores-preserved ()
  "Underscores are preserved in filenames."
  (should (equal "my_file" (gdocs--sanitize-filename "my_file"))))

(ert-deftest gdocs-test-sanitize-mixed-case-preserved ()
  "Mixed case is preserved."
  (should (equal "HelloWorld" (gdocs--sanitize-filename "HelloWorld"))))

;;;; Document URL construction

(ert-deftest gdocs-test-document-url-format ()
  "URL is constructed with the correct Google Docs edit path."
  (should (equal "https://docs.google.com/document/d/abc123/edit"
                 (gdocs--document-url "abc123"))))

(ert-deftest gdocs-test-document-url-preserves-id ()
  "Document ID is embedded verbatim in the URL."
  (let ((id "1BxiMVs0XRA5nFMdKvBdBZjgmUUqptlbs74OgVE2upms"))
    (should (string-match-p (regexp-quote id)
                            (gdocs--document-url id)))))

;;;; Buffer title extraction

(ert-deftest gdocs-test-buffer-title-from-org-keyword ()
  "Extracts #+TITLE from an org buffer."
  (with-temp-buffer
    (org-mode)
    (insert "#+TITLE: My Document\n* Heading\n")
    (should (equal "My Document" (gdocs--buffer-title)))))

(ert-deftest gdocs-test-buffer-title-from-filename ()
  "Falls back to file name without extension when no #+TITLE."
  (let ((temp-file (make-temp-file "gdocs-test-" nil ".org")))
    (unwind-protect
        (with-current-buffer (find-file-noselect temp-file)
          (org-mode)
          (insert "* Just a heading\n")
          (should (equal (file-name-sans-extension temp-file)
                         (gdocs--buffer-title)))
          (kill-buffer))
      (delete-file temp-file))))

(ert-deftest gdocs-test-buffer-title-from-buffer-name ()
  "Falls back to buffer name for unsaved buffers."
  (with-temp-buffer
    (org-mode)
    (insert "* No title keyword\n")
    ;; temp buffers have no file-name, so buffer-name is used
    (let ((expected (file-name-sans-extension (buffer-name))))
      (should (equal expected (gdocs--buffer-title))))))

;;;; Auto-activation

(ert-deftest gdocs-test-maybe-enable-activates-in-org-mode ()
  "Enables gdocs-mode when gdocs-document-id is set in org-mode."
  (with-temp-buffer
    (org-mode)
    (setq-local gdocs-document-id "test-doc-id")
    ;; Stub functions called by gdocs-mode--enable to avoid side effects
    (cl-letf (((symbol-function 'gdocs-sync--init-from-file-locals) #'ignore)
              ((symbol-function 'gdocs--update-modeline) #'ignore)
              ((symbol-function 'gdocs-sync-pull) #'ignore))
      (gdocs--maybe-enable)
      (should gdocs-mode))))

(ert-deftest gdocs-test-maybe-enable-skips-non-org ()
  "Does NOT enable gdocs-mode in non-org buffers."
  (with-temp-buffer
    (fundamental-mode)
    (setq-local gdocs-document-id "test-doc-id")
    (gdocs--maybe-enable)
    (should-not gdocs-mode)))

(ert-deftest gdocs-test-maybe-enable-skips-nil-id ()
  "Does NOT enable gdocs-mode when gdocs-document-id is nil."
  (with-temp-buffer
    (org-mode)
    (gdocs--maybe-enable)
    (should-not gdocs-mode)))

;;;; Keymap

(ert-deftest gdocs-test-keymap-push ()
  "C-c g p is bound to `gdocs-push'."
  (should (eq 'gdocs-push
              (lookup-key gdocs-mode-map (kbd "C-c g p")))))

(ert-deftest gdocs-test-keymap-pull ()
  "C-c g l is bound to `gdocs-pull'."
  (should (eq 'gdocs-pull
              (lookup-key gdocs-mode-map (kbd "C-c g l")))))

(ert-deftest gdocs-test-keymap-status ()
  "C-c g s is bound to `gdocs-status'."
  (should (eq 'gdocs-status
              (lookup-key gdocs-mode-map (kbd "C-c g s")))))

(ert-deftest gdocs-test-keymap-open-in-browser ()
  "C-c g o is bound to `gdocs-open-in-browser'."
  (should (eq 'gdocs-open-in-browser
              (lookup-key gdocs-mode-map (kbd "C-c g o")))))

(ert-deftest gdocs-test-keymap-unlink ()
  "C-c g u is bound to `gdocs-unlink'."
  (should (eq 'gdocs-unlink
              (lookup-key gdocs-mode-map (kbd "C-c g u")))))

;;;; File-local variable safety

(ert-deftest gdocs-test-document-id-safe-when-string ()
  "gdocs-document-id is safe as a file-local when its value is a string."
  (should (safe-local-variable-p 'gdocs-document-id "abc123")))

(ert-deftest gdocs-test-document-id-unsafe-when-non-string ()
  "gdocs-document-id is NOT safe as a file-local when its value is not a string."
  (should-not (safe-local-variable-p 'gdocs-document-id 42))
  (should-not (safe-local-variable-p 'gdocs-document-id nil)))

(ert-deftest gdocs-test-account-safe-when-string ()
  "gdocs-account is safe as a file-local when its value is a string."
  (should (safe-local-variable-p 'gdocs-account "personal")))

(ert-deftest gdocs-test-account-unsafe-when-non-string ()
  "gdocs-account is NOT safe as a file-local when its value is not a string."
  (should-not (safe-local-variable-p 'gdocs-account 42)))

(ert-deftest gdocs-test-revision-id-safe-when-string ()
  "gdocs-revision-id is safe as a file-local when its value is a string."
  (should (safe-local-variable-p 'gdocs-revision-id "ALm37BVTxyz123")))

(ert-deftest gdocs-test-revision-id-unsafe-when-non-string ()
  "gdocs-revision-id is NOT safe as a file-local when its value is not a string."
  (should-not (safe-local-variable-p 'gdocs-revision-id '(hack))))

(ert-deftest gdocs-test-last-sync-safe-when-string ()
  "gdocs-last-sync is safe as a file-local when its value is a string."
  (should (safe-local-variable-p 'gdocs-last-sync "2026-01-15T10:30:00+0000")))

(ert-deftest gdocs-test-last-sync-unsafe-when-non-string ()
  "gdocs-last-sync is NOT safe as a file-local when its value is not a string."
  (should-not (safe-local-variable-p 'gdocs-last-sync 1700000000)))

;;;; Mode enable/disable

(ert-deftest gdocs-test-enable-adds-after-save-hook ()
  "Enabling gdocs-mode adds `gdocs-sync--push-on-save' to `after-save-hook'."
  (with-temp-buffer
    (org-mode)
    (setq-local gdocs-document-id "test-doc-id")
    (cl-letf (((symbol-function 'gdocs-sync--init-from-file-locals) #'ignore)
              ((symbol-function 'gdocs--update-modeline) #'ignore)
              ((symbol-function 'gdocs-sync-pull) #'ignore))
      (gdocs-mode 1)
      (should (memq 'gdocs-sync--push-on-save
                     (buffer-local-value 'after-save-hook (current-buffer))))
      ;; Clean up
      (gdocs-mode -1))))

(ert-deftest gdocs-test-disable-removes-after-save-hook ()
  "Disabling gdocs-mode removes `gdocs-sync--push-on-save' from `after-save-hook'."
  (with-temp-buffer
    (org-mode)
    (setq-local gdocs-document-id "test-doc-id")
    (cl-letf (((symbol-function 'gdocs-sync--init-from-file-locals) #'ignore)
              ((symbol-function 'gdocs--update-modeline) #'ignore)
              ((symbol-function 'gdocs-sync-pull) #'ignore)
              ((symbol-function 'gdocs-sync--clear-buffer-state) #'ignore))
      (gdocs-mode 1)
      (gdocs-mode -1)
      (should-not (memq 'gdocs-sync--push-on-save
                         (buffer-local-value 'after-save-hook
                                             (current-buffer)))))))

;;;; Open in browser

(ert-deftest gdocs-test-open-in-browser-errors-when-unlinked ()
  "Signals a user-error when the buffer has no linked document."
  (with-temp-buffer
    (org-mode)
    (setq-local gdocs-sync--document-id nil)
    (should-error (gdocs-open-in-browser) :type 'user-error)))

(ert-deftest gdocs-test-open-in-browser-calls-browse-url ()
  "Calls `browse-url' with the correct document URL when linked."
  (with-temp-buffer
    (org-mode)
    (setq-local gdocs-sync--document-id "test-doc-id")
    (let ((captured-url nil))
      (cl-letf (((symbol-function 'browse-url)
                 (lambda (url) (setq captured-url url))))
        (gdocs-open-in-browser)
        (should (equal "https://docs.google.com/document/d/test-doc-id/edit"
                       captured-url))))))

(provide 'gdocs-test)
;;; gdocs-test.el ends here
