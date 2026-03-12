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
;;    (setq gdocs-accounts '(("personal" . ((client-id . "YOUR-ID")
;;                                           (client-secret . "YOUR-SECRET")))))
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

;;;; Customizable variables

(defcustom gdocs-auto-pull-on-open nil
  "Whether to automatically pull when opening a linked org file."
  :type 'boolean
  :group 'gdocs)

;;;; File-local variable safety

(put 'gdocs-document-id 'safe-local-variable #'stringp)
(put 'gdocs-account 'safe-local-variable #'stringp)
(put 'gdocs-revision-id 'safe-local-variable #'stringp)
(put 'gdocs-last-sync 'safe-local-variable #'stringp)

;;;; Modeline

(defvar-local gdocs--modeline-lighter " GDocs")

(defun gdocs--update-modeline ()
  "Update the modeline lighter based on sync status."
  (setq gdocs--modeline-lighter
        (format " GDocs:%s" (or gdocs-sync--status "off")))
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
      (erase-buffer)
      (insert org-string)
      (gdocs-sync--write-file-local-vars doc-id account)
      (save-buffer)
      (setq gdocs-sync--shadow-ir ir)
      (setq gdocs-sync--document-id doc-id)
      (setq gdocs-sync--account account)
      (gdocs-sync--update-last-sync-time)
      (gdocs-mode 1))
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
Replace problematic characters with hyphens and collapse runs."
  (let ((cleaned (replace-regexp-in-string "[^a-zA-Z0-9_.-]" "-" name)))
    (replace-regexp-in-string "-\\{2,\\}" "-" cleaned)))

;;;###autoload
(defun gdocs-create (&optional title account)
  "Create a new Google Doc from the current org buffer.
TITLE defaults to the buffer's #+TITLE or buffer name.  ACCOUNT
is the account name to use; if nil, prompt."
  (interactive)
  (let ((doc-title (or title (gdocs--buffer-title)))
        (acct (or account (gdocs-auth-select-account "Account: ")))
        (buf (current-buffer)))
    (gdocs-api-create-document
     doc-title
     (lambda (json)
       (with-current-buffer buf
         (gdocs--populate-created-document json acct)))
     acct)))

(defun gdocs--buffer-title ()
  "Return the #+TITLE of the current buffer, or the buffer name."
  (or (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "^#\\+TITLE:[ \t]+\\(.+\\)" nil t)
          (match-string-no-properties 1)))
      (file-name-sans-extension
       (or (buffer-file-name) (buffer-name)))))

(defun gdocs--populate-created-document (json account)
  "Populate the linked Google Doc after creation.
JSON is the create-document response.  ACCOUNT is the account
name."
  (let ((doc-id (alist-get 'documentId json))
        (ir (gdocs-convert-org-buffer-to-ir)))
    (gdocs-api-batch-update
     doc-id
     (gdocs-convert-ir-to-docs-requests ir)
     (lambda (_response)
       (with-current-buffer (current-buffer)
         (gdocs-sync--write-file-local-vars doc-id account)
         (setq gdocs-sync--shadow-ir ir)
         (setq gdocs-sync--document-id doc-id)
         (setq gdocs-sync--account account)
         (gdocs-sync--update-last-sync-time)
         (save-buffer)
         (gdocs-mode 1)
         (message "Created and linked Google Doc: %s" doc-id)))
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

(defun gdocs-status ()
  "Show sync status for the current buffer."
  (interactive)
  (message "Doc: %s | Account: %s | Status: %s | Rev: %s | Last sync: %s"
           (or gdocs-sync--document-id "none")
           (or gdocs-sync--account "none")
           (or gdocs-sync--status "off")
           (or gdocs-sync--revision-id "unknown")
           (or gdocs-sync--last-sync-time "never")))

(defun gdocs-open-in-browser ()
  "Open the linked Google Doc in the default web browser."
  (interactive)
  (unless gdocs-sync--document-id
    (user-error "Buffer is not linked to a Google Doc"))
  (browse-url (gdocs--document-url gdocs-sync--document-id)))

(defun gdocs--document-url (doc-id)
  "Return the Google Docs edit URL for DOC-ID."
  (format "https://docs.google.com/document/d/%s/edit" doc-id))

;;;; Auto-activation

(defun gdocs--maybe-enable ()
  "Enable `gdocs-mode' if the buffer has gdocs file-local variables."
  (when (and (derived-mode-p 'org-mode)
             (bound-and-true-p gdocs-document-id))
    (gdocs-mode 1)))

(add-hook 'org-mode-hook #'gdocs--maybe-enable)

(provide 'gdocs)
;;; gdocs.el ends here
