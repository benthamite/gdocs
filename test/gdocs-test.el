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
  (should (equal "caf" (gdocs--sanitize-filename "caf\u00e9")))
  (should (equal "untitled" (gdocs--sanitize-filename "\u00fc"))))

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

(ert-deftest gdocs-test-open-in-browser-dired-file ()
  "Opens linked doc from dired when point is on a linked file."
  (let* ((dir (make-temp-file "gdocs-test-" t))
         (file (expand-file-name "test.org" dir))
         (captured-url nil))
    (unwind-protect
        (progn
          (write-region
           (concat "* Test\n\n"
                   ";; Local Variables:\n"
                   ";; gdocs-document-id: \"dired-doc-id\"\n"
                   ";; End:\n")
           nil file)
          (with-current-buffer (dired dir)
            (dired-goto-file file)
            (cl-letf (((symbol-function 'browse-url)
                       (lambda (url) (setq captured-url url))))
              (gdocs-open-in-browser)
              (should (equal "https://docs.google.com/document/d/dired-doc-id/edit"
                             captured-url)))
            (kill-buffer)))
      (delete-directory dir t))))

(ert-deftest gdocs-test-open-in-browser-dired-directory ()
  "Opens linked folder from dired when point is on a linked directory."
  (let* ((parent (make-temp-file "gdocs-test-" t))
         (subdir (expand-file-name "sub" parent))
         (captured-url nil))
    (unwind-protect
        (progn
          (make-directory subdir)
          (write-region
           "((org-mode . ((gdocs-folder-id . \"test-folder-id\"))))\n"
           nil (expand-file-name ".dir-locals.el" subdir))
          (with-current-buffer (dired parent)
            (dired-goto-file (directory-file-name subdir))
            (cl-letf (((symbol-function 'browse-url)
                       (lambda (url) (setq captured-url url))))
              (gdocs-open-in-browser)
              (should (equal "https://drive.google.com/drive/folders/test-folder-id"
                             captured-url)))
            (kill-buffer)))
      (delete-directory parent t))))

(ert-deftest gdocs-test-open-in-browser-dired-unlinked-file ()
  "Signals user-error in dired when file at point is not linked."
  (let* ((dir (make-temp-file "gdocs-test-" t))
         (file (expand-file-name "plain.org" dir)))
    (unwind-protect
        (progn
          (write-region "* Plain file\n" nil file)
          (with-current-buffer (dired dir)
            (dired-goto-file file)
            (should-error (gdocs-open-in-browser) :type 'user-error)
            (kill-buffer)))
      (delete-directory dir t))))

;;;; Org tag management

(ert-deftest gdocs-test-ensure-org-tag-adds-to-bare-heading ()
  "Adds :gdocs: tag to a heading with no existing tags."
  (with-temp-buffer
    (org-mode)
    (insert "#+TITLE: Doc\n* Section One\nContent\n")
    (let ((gdocs-org-tag "gdocs"))
      (gdocs--ensure-org-tag))
    (goto-char (point-min))
    (forward-line 1)
    (should (string-match-p ":gdocs:" (buffer-substring
                                       (line-beginning-position)
                                       (line-end-position))))))

(ert-deftest gdocs-test-ensure-org-tag-appends-to-existing-tags ()
  "Appends :gdocs: to a heading that already has tags."
  (with-temp-buffer
    (org-mode)
    (insert "* Heading :foo:bar:\nContent\n")
    (let ((gdocs-org-tag "gdocs"))
      (gdocs--ensure-org-tag))
    (goto-char (point-min))
    (should (string-match-p ":foo:bar:gdocs:" (buffer-substring
                                                (line-beginning-position)
                                                (line-end-position))))))

(ert-deftest gdocs-test-ensure-org-tag-idempotent ()
  "Does not add the tag twice."
  (with-temp-buffer
    (org-mode)
    (insert "* Heading :gdocs:\nContent\n")
    (let ((gdocs-org-tag "gdocs"))
      (gdocs--ensure-org-tag))
    (goto-char (point-min))
    (let ((line (buffer-substring (line-beginning-position) (line-end-position))))
      ;; Count occurrences of :gdocs:
      (should (= 1 (with-temp-buffer
                      (insert line)
                      (goto-char (point-min))
                      (let ((count 0))
                        (while (search-forward ":gdocs:" nil t)
                          (cl-incf count))
                        count)))))))

(ert-deftest gdocs-test-ensure-org-tag-nil-disables ()
  "Does nothing when `gdocs-org-tag' is nil."
  (with-temp-buffer
    (org-mode)
    (insert "* Heading\nContent\n")
    (let ((gdocs-org-tag nil)
          (before (buffer-string)))
      (gdocs--ensure-org-tag)
      (should (equal before (buffer-string))))))

(ert-deftest gdocs-test-ensure-org-tag-no-heading ()
  "Does nothing in a buffer without headings."
  (with-temp-buffer
    (org-mode)
    (insert "#+TITLE: Doc\nJust text, no headings.\n")
    (let ((gdocs-org-tag "gdocs")
          (before (buffer-string)))
      (gdocs--ensure-org-tag)
      (should (equal before (buffer-string))))))

(ert-deftest gdocs-test-ensure-org-tag-custom-name ()
  "Uses the custom tag name."
  (with-temp-buffer
    (org-mode)
    (insert "* Heading\nContent\n")
    (let ((gdocs-org-tag "synced"))
      (gdocs--ensure-org-tag))
    (goto-char (point-min))
    (should (string-match-p ":synced:" (buffer-substring
                                        (line-beginning-position)
                                        (line-end-position))))))

(ert-deftest gdocs-test-remove-org-tag-only-tag ()
  "Removes entire tag decoration when it is the only tag."
  (with-temp-buffer
    (org-mode)
    (insert "* Heading :gdocs:\nContent\n")
    (let ((gdocs-org-tag "gdocs"))
      (gdocs--remove-org-tag))
    (goto-char (point-min))
    (should (equal "* Heading" (buffer-substring
                                (line-beginning-position)
                                (line-end-position))))))

(ert-deftest gdocs-test-remove-org-tag-preserves-others ()
  "Preserves other tags when removing the gdocs tag."
  (with-temp-buffer
    (org-mode)
    (insert "* Heading :foo:gdocs:bar:\nContent\n")
    (let ((gdocs-org-tag "gdocs"))
      (gdocs--remove-org-tag))
    (goto-char (point-min))
    (let ((line (buffer-substring (line-beginning-position)
                                  (line-end-position))))
      (should (string-match-p ":foo:bar:" line))
      (should-not (string-match-p ":gdocs:" line)))))

(ert-deftest gdocs-test-remove-org-tag-not-present ()
  "Does nothing when the tag is not present."
  (with-temp-buffer
    (org-mode)
    (insert "* Heading :other:\nContent\n")
    (let ((gdocs-org-tag "gdocs")
          (before (buffer-string)))
      (gdocs--remove-org-tag)
      (should (equal before (buffer-string))))))

(ert-deftest gdocs-test-remove-org-tag-nil-disables ()
  "Does nothing when `gdocs-org-tag' is nil."
  (with-temp-buffer
    (org-mode)
    (insert "* Heading :gdocs:\nContent\n")
    (let ((gdocs-org-tag nil)
          (before (buffer-string)))
      (gdocs--remove-org-tag)
      (should (equal before (buffer-string))))))

;;;; Folder URL construction

(ert-deftest gdocs-test-folder-url-format ()
  "URL has the correct Google Drive folders path."
  (should (equal "https://drive.google.com/drive/folders/abc123"
                 (gdocs--folder-url "abc123"))))

(ert-deftest gdocs-test-folder-url-preserves-id ()
  "Folder ID is embedded verbatim in the URL."
  (let ((id "1BxiMVs0XRA5nFMdKvBdBZjgmUUqptlbs74OgVE2upms"))
    (should (string-match-p (regexp-quote id)
                            (gdocs--folder-url id)))))

;;;; Dir-local reading (gdocs--dir-folder-id)

(ert-deftest gdocs-test-dir-folder-id-reads-from-dir-locals ()
  "Reads `gdocs-folder-id' from a `.dir-locals.el' file."
  (let ((dir (make-temp-file "gdocs-test-" t)))
    (unwind-protect
        (progn
          (write-region
           "((org-mode . ((gdocs-folder-id . \"folder-abc\"))))\n"
           nil (expand-file-name ".dir-locals.el" dir))
          (should (equal "folder-abc" (gdocs--dir-folder-id dir))))
      (delete-directory dir t))))

(ert-deftest gdocs-test-dir-folder-id-nil-when-no-dir-locals ()
  "Returns nil when no `.dir-locals.el' exists."
  (let ((dir (make-temp-file "gdocs-test-" t)))
    (unwind-protect
        (should-not (gdocs--dir-folder-id dir))
      (delete-directory dir t))))

(ert-deftest gdocs-test-dir-folder-id-nil-when-no-entry ()
  "Returns nil when `.dir-locals.el' has no `gdocs-folder-id'."
  (let ((dir (make-temp-file "gdocs-test-" t)))
    (unwind-protect
        (progn
          (write-region
           "((org-mode . ((fill-column . 80))))\n"
           nil (expand-file-name ".dir-locals.el" dir))
          (should-not (gdocs--dir-folder-id dir)))
      (delete-directory dir t))))

;;;; Dir-local reading (gdocs--dir-account)

(ert-deftest gdocs-test-dir-account-reads-from-dir-locals ()
  "Reads `gdocs-account' from a `.dir-locals.el' file."
  (let ((dir (make-temp-file "gdocs-test-" t)))
    (unwind-protect
        (progn
          (write-region
           "((org-mode . ((gdocs-account . \"personal\"))))\n"
           nil (expand-file-name ".dir-locals.el" dir))
          (should (equal "personal" (gdocs--dir-account dir))))
      (delete-directory dir t))))

(ert-deftest gdocs-test-dir-account-nil-when-no-dir-locals ()
  "Returns nil when no `.dir-locals.el' exists."
  (let ((dir (make-temp-file "gdocs-test-" t)))
    (unwind-protect
        (should-not (gdocs--dir-account dir))
      (delete-directory dir t))))

;;;; File document-id reading (gdocs--file-document-id)

(ert-deftest gdocs-test-file-document-id-reads-from-local-vars ()
  "Reads `gdocs-document-id' from file-local variables."
  (let ((file (make-temp-file "gdocs-test-" nil ".org")))
    (unwind-protect
        (progn
          (write-region
           (concat "* Test heading\n\n"
                   ";; Local Variables:\n"
                   ";; gdocs-document-id: \"doc-xyz-789\"\n"
                   ";; End:\n")
           nil file)
          (should (equal "doc-xyz-789" (gdocs--file-document-id file))))
      (delete-file file))))

(ert-deftest gdocs-test-file-document-id-nil-when-absent ()
  "Returns nil when no `gdocs-document-id' is present."
  (let ((file (make-temp-file "gdocs-test-" nil ".org")))
    (unwind-protect
        (progn
          (write-region "* Plain org file\nSome content.\n" nil file)
          (should-not (gdocs--file-document-id file)))
      (delete-file file))))

(ert-deftest gdocs-test-file-document-id-nil-when-no-local-vars ()
  "Returns nil when there is no Local Variables block at all."
  (let ((file (make-temp-file "gdocs-test-" nil ".org")))
    (unwind-protect
        (progn
          (write-region "#+TITLE: Just a title\n" nil file)
          (should-not (gdocs--file-document-id file)))
      (delete-file file))))

;;;; Effective dir-locals (gdocs--effective-dir-locals)

(ert-deftest gdocs-test-effective-dir-locals-walks-up ()
  "Finds a linked ancestor by walking up the directory tree."
  (let* ((parent (make-temp-file "gdocs-test-" t))
         (child (expand-file-name "sub/" parent)))
    (unwind-protect
        (progn
          (make-directory child t)
          (write-region
           "((org-mode . ((gdocs-folder-id . \"parent-folder\") (gdocs-account . \"work\"))))\n"
           nil (expand-file-name ".dir-locals.el" parent))
          (let ((default-directory (file-name-as-directory child)))
            (let ((result (gdocs--effective-dir-locals)))
              (should (equal "parent-folder" (car result)))
              (should (equal "work" (cdr result))))))
      (delete-directory parent t))))

(ert-deftest gdocs-test-effective-dir-locals-nil-when-none ()
  "Returns nil when no ancestor has a `.dir-locals.el' with a folder ID."
  (let ((dir (make-temp-file "gdocs-test-" t)))
    (unwind-protect
        (let ((default-directory (file-name-as-directory dir)))
          (should-not (gdocs--effective-dir-locals)))
      (delete-directory dir t))))

(ert-deftest gdocs-test-effective-dir-locals-prefers-nearest ()
  "Returns the nearest ancestor's dir-locals, not a more distant one."
  (let* ((grandparent (make-temp-file "gdocs-test-" t))
         (parent (expand-file-name "mid/" grandparent))
         (child (expand-file-name "leaf/" parent)))
    (unwind-protect
        (progn
          (make-directory child t)
          (write-region
           "((org-mode . ((gdocs-folder-id . \"gp-folder\") (gdocs-account . \"gp-acct\"))))\n"
           nil (expand-file-name ".dir-locals.el" grandparent))
          (write-region
           "((org-mode . ((gdocs-folder-id . \"parent-folder\") (gdocs-account . \"parent-acct\"))))\n"
           nil (expand-file-name ".dir-locals.el" parent))
          (let ((default-directory (file-name-as-directory child)))
            (let ((result (gdocs--effective-dir-locals)))
              (should (equal "parent-folder" (car result)))
              (should (equal "parent-acct" (cdr result))))))
      (delete-directory grandparent t))))

;;;; Ensure dir-locals file (gdocs--ensure-dir-locals-file)

(ert-deftest gdocs-test-ensure-dir-locals-file-creates-when-missing ()
  "Creates `.dir-locals.el' when it does not exist."
  (let* ((dir (make-temp-file "gdocs-test-" t))
         (dl-file (expand-file-name ".dir-locals.el" dir)))
    (unwind-protect
        (let ((default-directory (file-name-as-directory dir)))
          (should-not (file-exists-p dl-file))
          (gdocs--ensure-dir-locals-file)
          (should (file-exists-p dl-file)))
      (delete-directory dir t))))

(ert-deftest gdocs-test-ensure-dir-locals-file-noop-when-exists ()
  "Does not overwrite an existing `.dir-locals.el'."
  (let* ((dir (make-temp-file "gdocs-test-" t))
         (dl-file (expand-file-name ".dir-locals.el" dir))
         (contents "((org-mode . ((fill-column . 80))))\n"))
    (unwind-protect
        (let ((default-directory (file-name-as-directory dir)))
          (write-region contents nil dl-file)
          (gdocs--ensure-dir-locals-file)
          (should (equal contents
                         (with-temp-buffer
                           (insert-file-contents dl-file)
                           (buffer-string)))))
      (delete-directory dir t))))

;;;; Sanitize filename edge cases

(ert-deftest gdocs-test-sanitize-empty-string ()
  "Empty string input returns \"untitled\"."
  (should (equal "untitled" (gdocs--sanitize-filename ""))))

(ert-deftest gdocs-test-sanitize-only-special-chars ()
  "String of only special characters returns \"untitled\"."
  (should (equal "untitled" (gdocs--sanitize-filename "///***???")))
  (should (equal "untitled" (gdocs--sanitize-filename "   "))))

(ert-deftest gdocs-test-sanitize-leading-trailing-hyphens-trimmed ()
  "Leading and trailing hyphens are trimmed after sanitization."
  (should (equal "middle" (gdocs--sanitize-filename "-middle-")))
  (should (equal "ok" (gdocs--sanitize-filename "---ok---")))
  (should (equal "a" (gdocs--sanitize-filename " a "))))

(provide 'gdocs-test)
;;; gdocs-test.el ends here
