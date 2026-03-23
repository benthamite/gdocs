;;; gdocs.el --- Google Docs integration for Emacs org-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Pablo Stafforini

;; Author: gdocs contributors
;; Version: 0.2.0
;; Package-Requires: ((emacs "29.1") (plz "0.7") (dash "2.19") (s "1.13") (org "9.6"))
;; Keywords: docs, outlines, hypermedia
;; URL: https://github.com/benthamite/gdocs

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Bidirectional synchronization between org-mode files and Google Docs.
;; Open, edit, and push Google Docs as org-mode buffers.
;;
;; Quick start:
;; 1. Configure your Google Cloud OAuth credentials:
;;    (setopt gdocs-accounts '(("personal" . ((client-id . "YOUR-ID")
;;                                             (client-secret . "YOUR-SECRET")))))
;; 2. Authenticate: M-x gdocs-authenticate
;; 3. Open a Google Doc: M-x gdocs-open
;; 4. Or create one from an org file: M-x gdocs-create

;;; Code:

(require 'gdocs-auth)
(require 'gdocs-api)
(require 'gdocs-convert)
(require 'gdocs-diff)
(require 'gdocs-sync)
(require 'gdocs-merge)
(require 'transient)

(declare-function modify-dir-local-variable "files-x"
                  (mode variable value op))
(declare-function dired-get-filename "dired" (&optional localp no-error-if-not-filep))

;;;; Customizable variables

(defcustom gdocs-auto-pull-on-open nil
  "Whether to automatically pull when opening a linked org file."
  :type 'boolean
  :group 'gdocs)

(defcustom gdocs-org-tag "gdocs"
  "Tag to add to the first heading of linked org files.
When non-nil, this tag is added to the first heading of org files
that have a Google Doc counterpart.  Set to nil to disable."
  :type '(choice (const :tag "None" nil) string)
  :group 'gdocs)

;;;; Dir-local variable safety

(defvar gdocs-folder-id nil "Google Drive folder ID (dir-local).")

(put 'gdocs-folder-id 'safe-local-variable #'stringp)

;;;; Modeline

(defvar-local gdocs--modeline-lighter " GDocs")

(defun gdocs--update-modeline ()
  "Update the modeline lighter based on sync status."
  (setq gdocs--modeline-lighter
        (format " GDocs:%s" (or gdocs-sync--status 'off)))
  (force-mode-line-update))

;;;; Keymap

(defvar gdocs-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c g p") #'gdocs-push)
    (define-key map (kbd "C-c g l") #'gdocs-pull)
    (define-key map (kbd "C-c g s") #'gdocs-status)
    (define-key map (kbd "C-c g o") #'gdocs-open-in-browser)
    (define-key map (kbd "C-c g u") #'gdocs-unlink)
    map))

;;;; Minor mode

;;;###autoload
(define-minor-mode gdocs-mode
  "Minor mode for org buffers linked to Google Docs.
Enables push-on-save, modeline status display, and sync
keybindings."
  :lighter gdocs--modeline-lighter
  :keymap gdocs-mode-map
  (if gdocs-mode
      (gdocs-mode--enable)
    (gdocs-mode--disable)))

(defun gdocs-mode--enable ()
  "Set up gdocs-mode in the current buffer."
  (add-hook 'after-save-hook #'gdocs-sync--push-on-save nil t)
  (gdocs-sync--init-from-properties)
  (gdocs--update-modeline)
  (when gdocs-auto-pull-on-open
    (gdocs-sync-pull)))

(defun gdocs-mode--disable ()
  "Tear down gdocs-mode in the current buffer."
  (remove-hook 'after-save-hook #'gdocs-sync--push-on-save t)
  (gdocs-sync--clear-buffer-state)
  (setq gdocs--modeline-lighter " GDocs"))

;;;; User commands

;;;###autoload
(defun gdocs-open (document-id-or-url &optional account)
  "Open a Google Doc as an org buffer.
DOCUMENT-ID-OR-URL is a document ID or full Google Docs URL.
ACCOUNT is the account name to use; if nil, prompt."
  (interactive
   (progn
     (gdocs-auth--validate-accounts-configured)
     (list (read-string "Google Doc ID or URL: "))))
  (let ((doc-id (gdocs-sync--parse-document-id document-id-or-url))
        (acct (or account (gdocs-auth-select-account "Account: "))))
    (gdocs-api-get-document
     doc-id
     (lambda (json)
       (gdocs--open-document-from-json json doc-id acct))
     acct)))

(defun gdocs--open-document-from-json (json doc-id account)
  "Create an org buffer from document JSON.
DOC-ID is the document ID.  ACCOUNT is the account name."
  (gdocs-convert--cache-heading-ids doc-id json)
  (let* ((title (or (alist-get 'title json) "Untitled"))
         (gdocs-convert--document-id doc-id)
         (ir (gdocs-convert-docs-json-to-ir json))
         (org-string (gdocs-convert-ir-to-org ir))
         (file-path (gdocs--doc-file-path title))
         (buf (find-file-noselect file-path)))
    (with-current-buffer buf
      ;; Write content and set up properties
      (erase-buffer)
      (insert org-string)
      (gdocs--ensure-org-tag)
      ;; Initialize sync state before writing properties
      (setq gdocs-sync--document-id doc-id)
      (setq gdocs-sync--account account)
      (setq gdocs-sync--shadow-ir ir)
      (gdocs-sync--update-last-sync-time)
      (gdocs-sync--persist-properties)
      (let ((gdocs-auto-push-on-save nil))
        (save-buffer))
      (gdocs-mode 1))
    ;; Display to user
    (pop-to-buffer buf)
    (goto-char (point-min))
    (message "Opened: %s" title)))

(defun gdocs--doc-file-path (title)
  "Return the file path for a document with TITLE.
Creates `gdocs-directory' if it does not exist."
  (make-directory gdocs-directory t)
  (expand-file-name (concat (gdocs--sanitize-filename title) ".org")
                    gdocs-directory))

(defun gdocs--sanitize-filename (name)
  "Sanitize NAME for use as a filename.
Strips non-ASCII characters and filesystem-unsafe characters,
replacing them with hyphens.  Collapses consecutive hyphens.
Returns \"untitled\" if the result would be empty."
  (let* ((cleaned (replace-regexp-in-string "[^a-zA-Z0-9_.-]" "-" name))
         (collapsed (replace-regexp-in-string "-\\{2,\\}" "-" cleaned))
         (trimmed (string-trim collapsed "-+" "-+")))
    (if (string-empty-p trimmed) "untitled" trimmed)))

;;;###autoload
(defun gdocs-create (&optional title account)
  "Create a new Google Doc from the current org buffer.
TITLE defaults to the buffer's #+TITLE or buffer name.  ACCOUNT
is the account name to use; if nil, use the directory-local
account or prompt.  If the current directory is linked to a
Google Drive folder via `gdocs-folder-id', the new document is
placed in that folder."
  (interactive)
  (let* ((dir-locals (gdocs--effective-dir-locals))
         (doc-title (or title (gdocs--buffer-title)))
         (acct (or account
                   (cdr dir-locals)
                   gdocs-sync--account
                   (gdocs-auth-select-account "Account: ")))
         (folder-id (car dir-locals))
         (buf (current-buffer)))
    (gdocs-api-create-document
     doc-title
     (lambda (json)
       (with-current-buffer buf
         (gdocs--populate-created-document json acct folder-id)))
     acct)))

(defun gdocs--buffer-title ()
  "Return the #+TITLE of the current buffer, or the buffer name."
  (or (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "^#\\+TITLE:[ \t]+\\(.+\\)" nil t)
          (match-string-no-properties 1)))
      (file-name-sans-extension
       (or (buffer-file-name) (buffer-name)))))

(defun gdocs--populate-created-document (json account &optional folder-id)
  "Populate the linked Google Doc after creation.
JSON is the create-document response.  ACCOUNT is the account
name.  FOLDER-ID, if non-nil, specifies the Google Drive folder
to move the document into."
  (let ((doc-id (alist-get 'documentId json))
        (ir (gdocs-convert-org-buffer-to-ir))
        (buf (current-buffer)))
    ;; Move to the target folder if specified (fire-and-forget;
    ;; independent of the content push below).
    (when folder-id
      (gdocs-api-move-file
       doc-id folder-id
       (lambda (_) (message "Moved document to Google Drive folder"))
       account))
    ;; Push org content to the new document
    (gdocs-api-batch-update
     doc-id
     (gdocs-convert-ir-to-docs-requests
      (gdocs-sync--filter-title ir))
     (lambda (_response)
       ;; Fetch the actual document to build a shadow IR that matches
       ;; the real document structure (insertTable creates extra
       ;; paragraphs not present in the org buffer's IR).
       (gdocs-api-get-document doc-id
         (lambda (doc-json)
           (with-current-buffer buf
             (gdocs-convert--cache-heading-ids doc-id doc-json)
             (let* ((gdocs-convert--document-id doc-id)
                    (true-ir (gdocs-convert-docs-json-to-ir doc-json)))
               (setq gdocs-sync--shadow-ir true-ir)
               (setq gdocs-sync--document-id doc-id)
               (setq gdocs-sync--account account)
               (gdocs-sync--update-last-sync-time)
               (gdocs-sync--persist-properties)
               (gdocs--ensure-org-tag)
               (let ((gdocs-auto-push-on-save nil))
                 (save-buffer))
               (gdocs-mode 1)
               (message "Created and linked Google Doc: %s" doc-id))))
         account))
     account)))

(defun gdocs-push ()
  "Push local changes to the linked Google Doc."
  (interactive)
  (gdocs-sync-push))

(defun gdocs-pull ()
  "Pull remote changes from the linked Google Doc."
  (interactive)
  (gdocs-sync-pull))

(defun gdocs-link (document-id-or-url &optional account)
  "Link the current org buffer to an existing Google Doc.
DOCUMENT-ID-OR-URL is a document ID or full Google Docs URL.
ACCOUNT is the account name to use; if nil, prompt."
  (interactive
   (progn
     (gdocs-auth--validate-accounts-configured)
     (list (read-string "Google Doc ID or URL: "))))
  (gdocs-sync-link document-id-or-url account))

(defun gdocs-unlink ()
  "Unlink the current buffer from its Google Doc."
  (interactive)
  (gdocs-sync-unlink))

;;;###autoload
(defun gdocs-link-directory (folder-id &optional account)
  "Link the current directory to a Google Drive folder.
FOLDER-ID is a folder ID or Google Drive folder URL.  ACCOUNT is
the account name to use; if nil, prompt.  Writes the mapping to
`.dir-locals.el' so that `gdocs-create' automatically places new
documents in the linked folder and uses the linked account."
  (interactive
   (progn
     (gdocs-auth--validate-accounts-configured)
     (list (read-string "Google Drive folder ID or URL: "))))
  (let ((fid (gdocs-sync--parse-folder-id folder-id))
        (acct (or account (gdocs-auth-select-account "Account: "))))
    (gdocs--link-directory-noninteractive default-directory fid acct)
    (hack-dir-local-variables)
    (hack-local-variables-apply)
    (message "Linked %s to Google Drive folder %s" default-directory fid)))

(defun gdocs--link-directory-noninteractive (dir folder-id account)
  "Link DIR to Google Drive FOLDER-ID using ACCOUNT.
Writes the mapping to `.dir-locals.el' in DIR without any
interactive prompts or buffer-local side effects."
  (let ((default-directory (file-name-as-directory dir)))
    (gdocs--ensure-dir-locals-file)
    (setq dir-locals-directory-cache
          (assoc-delete-all default-directory dir-locals-directory-cache))
    (modify-dir-local-variable 'org-mode 'gdocs-folder-id folder-id 'add-or-replace)
    (modify-dir-local-variable 'org-mode 'gdocs-account account 'add-or-replace)
    (gdocs--save-dir-locals-buffer)))

(defun gdocs--ensure-dir-locals-file ()
  "Ensure a `.dir-locals.el' exists in `default-directory'."
  (let ((dl-file (expand-file-name ".dir-locals.el" default-directory)))
    (unless (file-exists-p dl-file)
      (write-region "" nil dl-file nil 'silent))))

;;;###autoload
(defun gdocs-unlink-directory ()
  "Unlink the current directory from its Google Drive folder.
Removes `gdocs-folder-id' and `gdocs-account' from
`.dir-locals.el'."
  (interactive)
  (let ((dl-file (expand-file-name ".dir-locals.el" default-directory)))
    (unless (file-exists-p dl-file)
      (user-error "No .dir-locals.el in %s" default-directory))
    ;; Edit the .dir-locals.el directly to avoid cache misdirection.
    (with-current-buffer (find-file-noselect dl-file)
      (let ((alist (save-excursion
                     (goto-char (point-min))
                     (ignore-errors (read (current-buffer))))))
        (when-let* ((org-entry (assq 'org-mode alist)))
          (setf (cdr org-entry)
                (assq-delete-all 'gdocs-folder-id
                                 (assq-delete-all 'gdocs-account
                                                  (cdr org-entry)))))
        (erase-buffer)
        (insert ";;; Directory Local Variables            -*- no-byte-compile: t -*-\n")
        (insert ";;; For more information see (info \"(emacs) Directory Variables\")\n\n")
        (pp alist (current-buffer))
        (save-buffer)
        (kill-buffer))))
  ;; Flush the dir-locals cache so buffers pick up the change.
  (setq dir-locals-directory-cache
        (assoc-delete-all (file-name-as-directory default-directory)
                          dir-locals-directory-cache))
  (kill-local-variable 'gdocs-folder-id)
  (kill-local-variable 'gdocs-account)
  (message "Unlinked %s from Google Drive folder" default-directory))

;;;###autoload
(defun gdocs-create-folder ()
  "Create a Google Drive folder for a local directory.
Prompt for a directory, defaulting to `default-directory' or the
directory at point in `dired-mode'.  The target must be inside a
directory already linked to a Google Drive folder.

If there are unlinked intermediate directories between the target
and the nearest linked ancestor, prompt before creating them."
  (interactive)
  (gdocs-auth--validate-accounts-configured)
  (let* ((target-dir (gdocs--create-folder-read-target))
         (existing-id (gdocs--dir-folder-id target-dir)))
    (when existing-id
      (user-error "Directory %s is already linked (folder ID: %s)"
                  target-dir existing-id))
    (let* ((link-info (gdocs--find-linked-ancestor target-dir))
           (linked-dir (nth 0 link-info))
           (parent-folder-id (nth 1 link-info))
           (account (nth 2 link-info))
           (dirs-to-create (gdocs--intermediate-dirs linked-dir target-dir)))
      (gdocs--confirm-intermediate-dirs dirs-to-create)
      (gdocs--create-folders-chain
       dirs-to-create parent-folder-id account
       (lambda (_final-id)
         (message "Created Google Drive folder%s for %s"
                  (if (> (length dirs-to-create) 1) "s" "")
                  target-dir))))))

(defun gdocs--create-folder-read-target ()
  "Prompt for the target directory for `gdocs-create-folder'.
Default to the directory at point in `dired-mode', or
`default-directory' otherwise."
  (let ((default (if (derived-mode-p 'dired-mode)
                     (let ((file (dired-get-filename nil t)))
                       (when (and file (file-directory-p file))
                         (file-name-as-directory file)))
                   default-directory)))
    (file-name-as-directory
     (read-directory-name "Create Google Drive folder for: " default nil t))))

(defun gdocs--find-linked-ancestor (dir)
  "Find the nearest proper ancestor of DIR linked to Google Drive.
Return a list (LINKED-DIR FOLDER-ID ACCOUNT).  Signal an error
if no linked ancestor is found.  DIR itself is not considered."
  (let* ((dir (file-name-as-directory (expand-file-name dir)))
         (current (file-name-directory (directory-file-name dir)))
         folder-id)
    (while (and current (not (setq folder-id (gdocs--dir-folder-id current))))
      (let ((parent (file-name-directory (directory-file-name current))))
        (if (equal parent current)
            (setq current nil)
          (setq current parent))))
    (unless folder-id
      (user-error "No linked ancestor directory found for %s" dir))
    (list (file-name-as-directory current)
          folder-id
          (gdocs--dir-account current))))

(defun gdocs--intermediate-dirs (ancestor-dir target-dir)
  "Return directories between ANCESTOR-DIR and TARGET-DIR.
Each entry is (DIR . NAME) where DIR is the full path and NAME
is the directory basename for the Google Drive folder.
ANCESTOR-DIR must be a proper ancestor of TARGET-DIR.  The
returned list includes TARGET-DIR but excludes ANCESTOR-DIR."
  (let* ((ancestor (file-name-as-directory (expand-file-name ancestor-dir)))
         (target (file-name-as-directory (expand-file-name target-dir)))
         (relative (string-remove-prefix ancestor target))
         (components (split-string (directory-file-name relative) "/" t))
         (current ancestor)
         result)
    (dolist (name components)
      (setq current (file-name-as-directory (expand-file-name name current)))
      (push (cons current name) result))
    (nreverse result)))

(defun gdocs--confirm-intermediate-dirs (dirs)
  "Prompt the user if DIRS contains intermediate directories.
DIRS is the list from `gdocs--intermediate-dirs'."
  (when (> (length dirs) 1)
    (let ((intermediates (mapcar #'cdr (butlast dirs))))
      (unless (yes-or-no-p
               (format "Also create intermediate folder%s %s? "
                       (if (> (length intermediates) 1) "s" "")
                       (mapconcat (lambda (n) (format "`%s'" n))
                                  intermediates ", ")))
        (user-error "Aborted")))))

(defun gdocs--create-folders-chain (dirs parent-folder-id account callback)
  "Create Google Drive folders for DIRS sequentially.
DIRS is a list of (LOCAL-DIR . FOLDER-NAME) pairs.  Each folder
is created as a child of the previous, starting with
PARENT-FOLDER-ID.  ACCOUNT is the Google account name.  CALLBACK
is called with the final folder ID when all folders have been
created and linked locally."
  (if (null dirs)
      (funcall callback parent-folder-id)
    (let* ((entry (car dirs))
           (local-dir (car entry))
           (folder-name (cdr entry)))
      (gdocs-api-create-folder
       folder-name
       (lambda (json)
         (let ((new-id (alist-get 'id json)))
           (gdocs--link-directory-noninteractive local-dir new-id account)
           (gdocs--create-folders-chain
            (cdr dirs) new-id account callback)))
       account
       parent-folder-id))))

(defun gdocs--save-dir-locals-buffer ()
  "Save and kill the `.dir-locals.el' buffer if visiting."
  (when-let* ((dir-locals-file (expand-file-name ".dir-locals.el"
                                                  default-directory))
              (buf (find-buffer-visiting dir-locals-file)))
    (with-current-buffer buf
      (save-buffer)
      (kill-buffer))))

(defun gdocs-status ()
  "Show sync status for the current buffer."
  (interactive)
  (message "Doc: %s | Account: %s | Status: %s | Rev: %s | Last sync: %s"
           (or gdocs-sync--document-id "none")
           (or gdocs-sync--account "none")
           (or gdocs-sync--status 'off)
           (or gdocs-sync--revision-id "unknown")
           (or gdocs-sync--last-sync-time "never")))

(defun gdocs-open-in-browser ()
  "Open the linked Google Doc or folder in the default web browser.
In a `dired' buffer, operate on the file or directory at point."
  (interactive)
  (if (derived-mode-p 'dired-mode)
      (gdocs--open-dired-entry-in-browser)
    (unless gdocs-sync--document-id
      (user-error "Buffer is not linked to a Google Doc"))
    (browse-url (gdocs--document-url gdocs-sync--document-id))))

(defun gdocs--open-dired-entry-in-browser ()
  "Open the Google Doc or folder linked to the dired entry at point."
  (let ((file (dired-get-filename nil t)))
    (unless file
      (user-error "No file at point"))
    (if (file-directory-p file)
        (let ((folder-id (gdocs--dir-folder-id file)))
          (unless folder-id
            (user-error "Directory is not linked to a Google Drive folder"))
          (browse-url (gdocs--folder-url folder-id)))
      (let ((doc-id (gdocs--file-document-id file)))
        (unless doc-id
          (user-error "File is not linked to a Google Doc"))
        (browse-url (gdocs--document-url doc-id))))))

(defun gdocs--file-document-id (file)
  "Return the GDOCS_DOCUMENT_ID property from FILE, or nil.
Only reads the first 1KB of the file (where the property drawer
lives) to avoid loading large org files into memory."
  (gdocs-convert--read-file-property-gdocs-id file))

(defun gdocs--read-dir-local-variable (dir variable)
  "Return the org-mode DIR-local VARIABLE from DIR's `.dir-locals.el', or nil."
  (let ((dl-file (expand-file-name ".dir-locals.el" dir)))
    (when (file-exists-p dl-file)
      (with-temp-buffer
        (insert-file-contents dl-file)
        (let ((alist (ignore-errors (read (current-buffer)))))
          (when-let* ((org-entry (alist-get 'org-mode alist)))
            (alist-get variable org-entry)))))))

(defun gdocs--dir-folder-id (dir)
  "Return the `gdocs-folder-id' dir-local variable for DIR, or nil."
  (gdocs--read-dir-local-variable dir 'gdocs-folder-id))

(defun gdocs--dir-account (dir)
  "Return the `gdocs-account' dir-local variable for DIR, or nil."
  (gdocs--read-dir-local-variable dir 'gdocs-account))

(defun gdocs--effective-dir-locals ()
  "Return the effective (FOLDER-ID . ACCOUNT) for `default-directory'.
Walk up from `default-directory' reading `.dir-locals.el' files
on disk.  This avoids relying on potentially stale buffer-local
values that were set before a `.dir-locals.el' was created."
  (let ((dir (file-name-as-directory default-directory))
        folder-id)
    (while (and dir (not (setq folder-id (gdocs--dir-folder-id dir))))
      (let ((parent (file-name-directory (directory-file-name dir))))
        (if (equal parent dir)
            (setq dir nil)
          (setq dir parent))))
    (when folder-id
      (cons folder-id (gdocs--dir-account dir)))))

(defun gdocs--document-url (doc-id)
  "Return the Google Docs edit URL for DOC-ID."
  (format "https://docs.google.com/document/d/%s/edit" doc-id))

(defun gdocs--folder-url (folder-id)
  "Return the Google Drive folder URL for FOLDER-ID."
  (format "https://drive.google.com/drive/folders/%s" folder-id))

;;;; Migration from file-local variables

;;;###autoload
(defun gdocs-migrate-file-locals-to-properties (&optional file)
  "Migrate gdocs metadata from file-local variables to a property drawer.
When called interactively, operate on the current buffer.  When
called from Lisp with FILE, operate on that file non-interactively.
Returns non-nil if migration was performed."
  (interactive)
  (let ((buf (if file (find-file-noselect file) (current-buffer))))
    (with-current-buffer buf
      (save-excursion
        (goto-char (point-max))
        (when (re-search-backward "^# Local Variables:" nil t)
          (let ((doc-id nil) (account nil) (rev-id nil) (last-sync nil))
            ;; Extract values from Local Variables block
            (save-excursion
              (let ((end (save-excursion
                           (re-search-forward "^# End:" nil t)
                           (point))))
                (when (re-search-forward
                       "# gdocs-document-id:[[:space:]]+\"\\([^\"]+\\)\"" end t)
                  (setq doc-id (match-string 1)))
                (goto-char (point))
                (when (re-search-backward "^# Local Variables:" nil t)
                  (when (re-search-forward
                         "# gdocs-account:[[:space:]]+\"\\([^\"]+\\)\"" end t)
                    (setq account (match-string 1)))
                  (goto-char (point))
                  (when (re-search-backward "^# Local Variables:" nil t)
                    (when (re-search-forward
                           "# gdocs-revision-id:[[:space:]]+\"\\([^\"]+\\)\"" end t)
                      (setq rev-id (match-string 1)))
                    (goto-char (point))
                    (when (re-search-backward "^# Local Variables:" nil t)
                      (when (re-search-forward
                             "# gdocs-last-sync:[[:space:]]+\"\\([^\"]+\\)\"" end t)
                        (setq last-sync (match-string 1))))))))
            (when doc-id
              ;; Remove the Local Variables block
              (goto-char (point-max))
              (when (re-search-backward "^# Local Variables:" nil t)
                ;; Include preceding blank lines
                (let ((block-start (point)))
                  (forward-line -1)
                  (while (and (> (point) (point-min))
                              (looking-at-p "^[[:space:]]*$"))
                    (forward-line -1))
                  (unless (looking-at-p "^[[:space:]]*$")
                    (forward-line 1))
                  (setq block-start (point))
                  (goto-char (point-max))
                  (delete-region block-start (point-max))))
              ;; Write property drawer at top
              (goto-char (point-min))
              (org-entry-put nil "GDOCS_DOCUMENT_ID" doc-id)
              (when account
                (org-entry-put nil "GDOCS_ACCOUNT" account))
              (when rev-id
                (org-entry-put nil "GDOCS_REVISION_ID" rev-id))
              (when last-sync
                (org-entry-put nil "GDOCS_LAST_SYNC" last-sync))
              (save-buffer)
              (message "Migrated %s" (or file (buffer-name)))
              t)))))))

;;;###autoload
(defun gdocs-migrate-directory (directory)
  "Migrate all org files with old-format gdocs metadata in DIRECTORY.
Scans for files containing a `Local Variables' block with
`gdocs-document-id' and migrates each to a property drawer."
  (interactive "DMigrate directory: ")
  (let ((files (directory-files directory t "\\.org\\'"))
        (count 0))
    (dolist (file files)
      (when (with-temp-buffer
              (insert-file-contents file nil
                                    (max 0 (- (file-attribute-size
                                               (file-attributes file))
                                              3072)))
              (goto-char (point-min))
              (re-search-forward "gdocs-document-id:" nil t))
        (when (gdocs-migrate-file-locals-to-properties file)
          (cl-incf count))))
    (message "Migrated %d file%s in %s" count
             (if (= count 1) "" "s") directory)))

;;;; Auto-activation

(defun gdocs--maybe-enable ()
  "Enable `gdocs-mode' if the buffer has a GDOCS_DOCUMENT_ID property."
  (when (and (derived-mode-p 'org-mode)
             (save-excursion
               (goto-char (point-min))
               (org-entry-get nil "GDOCS_DOCUMENT_ID")))
    (gdocs-mode 1)))

(add-hook 'org-mode-hook #'gdocs--maybe-enable)

;;;; Org tag management

(defun gdocs--ensure-org-tag ()
  "Ensure `gdocs-org-tag' is on the first heading in the buffer.
Does nothing when `gdocs-org-tag' is nil or the buffer has no
headings."
  (when gdocs-org-tag
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward org-heading-regexp nil t)
        (let* ((eol (line-end-position))
               (line (buffer-substring-no-properties
                      (line-beginning-position) eol))
               (tag-re (concat ":" (regexp-quote gdocs-org-tag) ":"))
               ;; Detect existing org tags: require whitespace before
               ;; the tag string to avoid false positives on headings
               ;; whose title ends with a literal colon.
               (has-tags (string-match-p
                          "[ \t]:[[:alnum:]_@#%:]+:[ \t]*$" line)))
          (unless (string-match-p tag-re line)
            (goto-char eol)
            (skip-chars-backward " \t")
            (delete-region (point) eol)
            (if has-tags
                ;; Existing tags: append ours before the final colon
                (insert (concat gdocs-org-tag ":"))
              ;; No tags: add tag decoration
              (insert (format " :%s:" gdocs-org-tag)))))))))

(defun gdocs--remove-org-tag ()
  "Remove `gdocs-org-tag' from the first heading in the buffer.
Does nothing when `gdocs-org-tag' is nil or the tag is not
present."
  (when gdocs-org-tag
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward org-heading-regexp nil t)
        (end-of-line)
        (when (re-search-backward
               "\\([ \t]+\\)\\(:[[:alnum:]_@#%:]+:\\)[ \t]*$"
               (line-beginning-position) t)
          ;; Save match bounds before `split-string' overwrites
          ;; the global match data.
          (let* ((mbeg (match-beginning 0))
                 (mend (match-end 0))
                 (space (match-string 1))
                 (tag-string (match-string 2))
                 (tags (split-string tag-string ":" t))
                 (remaining (remove gdocs-org-tag tags)))
            (when (< (length remaining) (length tags))
              (delete-region mbeg mend)
              (when remaining
                (goto-char mbeg)
                (insert space ":" (mapconcat #'identity remaining ":")
                        ":")))))))))

;;;; Transient menu

(transient-define-infix gdocs-menu--auto-push ()
  "Toggle automatic push on save."
  :class 'transient-lisp-variable
  :variable 'gdocs-auto-push-on-save
  :description "Auto-push on save"
  :reader (lambda (&rest _) (not gdocs-auto-push-on-save)))

(transient-define-infix gdocs-menu--auto-pull ()
  "Toggle automatic pull on open."
  :class 'transient-lisp-variable
  :variable 'gdocs-auto-pull-on-open
  :description "Auto-pull on open"
  :reader (lambda (&rest _) (not gdocs-auto-pull-on-open)))

(transient-define-infix gdocs-menu--directory ()
  "Set the default directory for synced org files."
  :class 'transient-lisp-variable
  :variable 'gdocs-directory
  :description "Sync directory"
  :reader (lambda (&rest _)
            (read-directory-name "Sync directory: " gdocs-directory)))

(transient-define-infix gdocs-menu--org-tag ()
  "Set the org tag for linked files."
  :class 'transient-lisp-variable
  :variable 'gdocs-org-tag
  :description "Org tag"
  :reader (lambda (&rest _)
            (let ((val (read-string "Org tag (empty for none): "
                                    gdocs-org-tag)))
              (if (string-empty-p val) nil val))))

(transient-define-infix gdocs-menu--link-directories ()
  "Set additional directories for cross-document link resolution."
  :class 'transient-lisp-variable
  :variable 'gdocs-link-directories
  :description "Link directories"
  :reader (lambda (&rest _)
            (let ((dirs gdocs-link-directories)
                  dir)
              (while (not (string-empty-p
                           (setq dir (read-directory-name
                                      "Add directory (empty to finish): "
                                      nil "" t))))
                (cl-pushnew (file-name-as-directory dir) dirs :test #'equal))
              dirs)))

;;;###autoload (autoload 'gdocs-menu "gdocs" nil t)
(transient-define-prefix gdocs-menu ()
  "Transient menu for gdocs."
  [["Sync"
    ("p" "Push" gdocs-push)
    ("l" "Pull" gdocs-pull)
    ("s" "Status" gdocs-status)]
   ["Documents"
    ("o" "Open" gdocs-open)
    ("c" "Create" gdocs-create)
    ("b" "Open in browser" gdocs-open-in-browser)]]
  [["Linking"
    ("L" "Link buffer" gdocs-link)
    ("U" "Unlink buffer" gdocs-unlink)
    ("D" "Link directory" gdocs-link-directory)
    ("X" "Unlink directory" gdocs-unlink-directory)
    ("F" "Create folder" gdocs-create-folder)]
   ["Auth"
    ("a" "Authenticate" gdocs-authenticate)
    ("x" "Logout" gdocs-logout)]]
  ["Options"
   ("-p" gdocs-menu--auto-push)
   ("-l" gdocs-menu--auto-pull)
   ("-d" gdocs-menu--directory)
   ("-t" gdocs-menu--org-tag)
   ("-L" gdocs-menu--link-directories)])

(provide 'gdocs)
;;; gdocs.el ends here
