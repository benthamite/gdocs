;;; gdocs.el --- Google Docs integration for Emacs org-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Pablo Stafforini

;; Author: gdocs contributors
;; Version: 0.1.0
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

;;;; File-local variable safety

;; File-local variables set in the Local Variables block of linked
;; org files.  Declared here so `bound-and-true-p' checks have a
;; known symbol and grep can find the definitions.
(defvar gdocs-document-id nil "Google Docs document ID (file-local).")
(defvar gdocs-account nil "Google account name (file-local).")
(defvar gdocs-revision-id nil "Last known Drive revision ID (file-local).")
(defvar gdocs-last-sync nil "ISO 8601 timestamp of last sync (file-local).")
(defvar gdocs-folder-id nil "Google Drive folder ID (dir-local).")

(put 'gdocs-document-id 'safe-local-variable #'stringp)
(put 'gdocs-account 'safe-local-variable #'stringp)
(put 'gdocs-revision-id 'safe-local-variable #'stringp)
(put 'gdocs-last-sync 'safe-local-variable #'stringp)
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
  (gdocs-sync--init-from-file-locals)
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
  (let* ((title (or (alist-get 'title json) "Untitled"))
         (ir (gdocs-convert-docs-json-to-ir json))
         (org-string (gdocs-convert-ir-to-org ir))
         (file-path (gdocs--doc-file-path title))
         (buf (find-file-noselect file-path)))
    (with-current-buffer buf
      ;; Write content and set up file-local variables
      (erase-buffer)
      (insert org-string)
      (gdocs--ensure-org-tag)
      (gdocs-sync--write-file-local-vars doc-id account)
      (setq gdocs-sync--shadow-ir ir)
      (let ((gdocs-auto-push-on-save nil))
        (save-buffer))
      ;; Initialize sync state
      (setq gdocs-sync--document-id doc-id)
      (setq gdocs-sync--account account)
      (gdocs-sync--update-last-sync-time)
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
replacing them with hyphens.  Collapses consecutive hyphens."
  (let ((cleaned (replace-regexp-in-string "[^a-zA-Z0-9_.-]" "-" name)))
    (replace-regexp-in-string "-\\{2,\\}" "-" cleaned)))

;;;###autoload
(defun gdocs-create (&optional title account)
  "Create a new Google Doc from the current org buffer.
TITLE defaults to the buffer's #+TITLE or buffer name.  ACCOUNT
is the account name to use; if nil, use the directory-local
account or prompt.  If the current directory is linked to a
Google Drive folder via `gdocs-folder-id', the new document is
placed in that folder."
  (interactive)
  (let ((doc-title (or title (gdocs--buffer-title)))
        (acct (or account
                  (bound-and-true-p gdocs-account)
                  (gdocs-auth-select-account "Account: ")))
        (folder-id (bound-and-true-p gdocs-folder-id))
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
             (let ((true-ir (gdocs-convert-docs-json-to-ir doc-json)))
               (gdocs-sync--write-file-local-vars doc-id account)
               (gdocs--ensure-org-tag)
               (setq gdocs-sync--shadow-ir true-ir)
               (setq gdocs-sync--document-id doc-id)
               (setq gdocs-sync--account account)
               (gdocs-sync--update-last-sync-time)
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
        (acct (or account (gdocs-auth-select-account "Account: ")))
        (dir default-directory))
    ;; Ensure a `.dir-locals.el' exists in the current directory so
    ;; that `modify-dir-local-variable' edits THIS file rather than a
    ;; parent directory's.
    (let ((dl-file (expand-file-name ".dir-locals.el" dir)))
      (unless (file-exists-p dl-file)
        (write-region "" nil dl-file nil 'silent)))
    ;; Flush the dir-locals cache so `dir-locals-find-file' discovers
    ;; the local `.dir-locals.el' instead of returning a cached parent.
    (setq dir-locals-directory-cache
          (assoc-delete-all dir dir-locals-directory-cache))
    (modify-dir-local-variable 'org-mode 'gdocs-folder-id fid 'add-or-replace)
    (modify-dir-local-variable 'org-mode 'gdocs-account acct 'add-or-replace)
    (gdocs--save-dir-locals-buffer)
    ;; Apply dir-locals to the current buffer immediately.
    (hack-dir-local-variables)
    (hack-local-variables-apply)
    (message "Linked %s to Google Drive folder %s" dir fid)))

;;;###autoload
(defun gdocs-unlink-directory ()
  "Unlink the current directory from its Google Drive folder.
Removes `gdocs-folder-id' and `gdocs-account' from
`.dir-locals.el'."
  (interactive)
  ;; Flush the dir-locals cache so we modify the current directory's
  ;; `.dir-locals.el', not a parent's.
  (setq dir-locals-directory-cache
        (assoc-delete-all default-directory dir-locals-directory-cache))
  (modify-dir-local-variable 'org-mode 'gdocs-folder-id nil 'delete)
  (modify-dir-local-variable 'org-mode 'gdocs-account nil 'delete)
  (gdocs--save-dir-locals-buffer)
  (kill-local-variable 'gdocs-folder-id)
  (kill-local-variable 'gdocs-account)
  (message "Unlinked %s from Google Drive folder" default-directory))

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
  "Return the `gdocs-document-id' file-local variable from FILE, or nil."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-max))
    (when (search-backward "Local Variables:" nil t)
      (let ((case-fold-search t))
        (when (re-search-forward
               "gdocs-document-id:[[:space:]]+\"\\([^\"]+\\)\"" nil t)
          (match-string 1))))))

(defun gdocs--dir-folder-id (dir)
  "Return the `gdocs-folder-id' dir-local variable for DIR, or nil."
  (let ((dl-file (expand-file-name ".dir-locals.el" dir)))
    (when (file-exists-p dl-file)
      (with-temp-buffer
        (insert-file-contents dl-file)
        (let ((alist (ignore-errors (read (current-buffer)))))
          (when-let* ((org-entry (alist-get 'org-mode alist)))
            (alist-get 'gdocs-folder-id org-entry)))))))

(defun gdocs--document-url (doc-id)
  "Return the Google Docs edit URL for DOC-ID."
  (format "https://docs.google.com/document/d/%s/edit" doc-id))

(defun gdocs--folder-url (folder-id)
  "Return the Google Drive folder URL for FOLDER-ID."
  (format "https://drive.google.com/drive/folders/%s" folder-id))

;;;; Auto-activation

(defun gdocs--maybe-enable ()
  "Enable `gdocs-mode' if the buffer has gdocs file-local variables."
  (when (and (derived-mode-p 'org-mode)
             (bound-and-true-p gdocs-document-id))
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
               (tag-re (concat ":" (regexp-quote gdocs-org-tag) ":")))
          (unless (string-match-p tag-re line)
            (goto-char eol)
            (skip-chars-backward " \t")
            (delete-region (point) eol)
            (if (eq (char-before) ?:)
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

(provide 'gdocs)
;;; gdocs.el ends here
