;;; gdocs-sync.el --- Push/pull synchronization for gdocs -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Pablo Stafforini

;; Author: gdocs contributors

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Push/pull orchestration, shadow copy management, and conflict
;; detection for bidirectional sync between org buffers and Google
;; Docs.  The shadow IR tracks the last-synced state to enable
;; incremental diff-based pushes and conflict detection on pull.

;;; Code:

(require 'cl-lib)
(require 'gdocs-auth)
(require 'gdocs-api)
(require 'gdocs-convert)
(require 'gdocs-diff)

(declare-function gdocs-merge-start "gdocs-merge")
(declare-function gdocs--update-modeline "gdocs")

;;;; Customizable variables

(defcustom gdocs-directory (expand-file-name "~/org/gdocs/")
  "Default directory for synced org files."
  :type 'directory
  :group 'gdocs)

(defcustom gdocs-auto-push-on-save t
  "Whether to automatically push changes on save."
  :type 'boolean
  :group 'gdocs)

;;;; Buffer-local variables

(defvar-local gdocs-sync--shadow-ir nil
  "IR of the last-synced state.")

(defvar-local gdocs-sync--document-id nil
  "Google Docs document ID for this buffer.")

(defvar-local gdocs-sync--account nil
  "Account name for this buffer.")

(defvar-local gdocs-sync--revision-id nil
  "Last known revision ID from Google Drive.")

(defvar-local gdocs-sync--last-sync-time nil
  "ISO 8601 timestamp of the last sync.")

(defvar-local gdocs-sync--push-in-progress nil
  "Non-nil when a push is currently in progress.")

(defvar-local gdocs-sync--push-queued nil
  "Non-nil when a push is queued behind an in-progress push.")

(defvar-local gdocs-sync--status 'synced
  "Current sync status.
One of `synced', `modified', `pushing', `conflict', or `error'.")

;;;; Push

(defun gdocs-sync-push ()
  "Push local changes to the linked Google Doc."
  (interactive)
  (unless gdocs-sync--document-id
    (user-error "Buffer is not linked to a Google Doc"))
  (unless (gdocs-sync--serialize-push)
    (setq gdocs-sync--push-in-progress t)
    (gdocs-sync--set-status 'pushing)
    (let ((current-ir (gdocs-convert-org-buffer-to-ir))
          (buf (current-buffer)))
      (if (null gdocs-sync--shadow-ir)
          (gdocs-sync--push-full-replacement current-ir buf)
        (gdocs-sync--push-incremental current-ir buf)))))

(defun gdocs-sync--serialize-push ()
  "If a push is already in progress, queue this one.
Return non-nil if the push was serialized (caller should abort)."
  (when gdocs-sync--push-in-progress
    (setq gdocs-sync--push-queued t)
    (message "Push already in progress, queued")
    t))

(defun gdocs-sync--push-full-replacement (current-ir buf)
  "Push CURRENT-IR as a full document replacement.
BUF is the originating buffer."
  (let ((requests (gdocs-convert-ir-to-docs-requests
                   (gdocs-sync--filter-title current-ir))))
    (gdocs-api-batch-update
     gdocs-sync--document-id
     requests
     (gdocs-sync--make-push-callback current-ir buf)
     gdocs-sync--account
     (gdocs-sync--make-push-error-callback buf))))

(defun gdocs-sync--push-incremental (current-ir buf)
  "Push CURRENT-IR as an incremental diff against the shadow.
BUF is the originating buffer."
  (let* ((start-index (gdocs-sync--body-start-index gdocs-sync--shadow-ir))
         (requests (gdocs-diff-generate
                    (gdocs-sync--filter-title gdocs-sync--shadow-ir)
                    (gdocs-sync--filter-title current-ir)
                    start-index)))
    (if (null requests)
        (gdocs-sync--push-no-changes buf)
      (gdocs-api-batch-update
       gdocs-sync--document-id
       requests
       (gdocs-sync--make-push-callback current-ir buf)
       gdocs-sync--account
       (gdocs-sync--make-push-error-callback buf)))))

(defun gdocs-sync--push-no-changes (buf)
  "Handle the case where push found no changes.
BUF is the originating buffer."
  (with-current-buffer buf
    (setq gdocs-sync--push-in-progress nil)
    (gdocs-sync--set-status 'synced)
    (message "No changes to push")))

(defun gdocs-sync--make-push-callback (current-ir buf)
  "Return a callback for a push API response.
CURRENT-IR is the IR that was pushed.  BUF is the originating
buffer."
  (lambda (response)
    (condition-case err
        (with-current-buffer buf
          (gdocs-sync--handle-push-success current-ir response))
      (error
       (with-current-buffer buf
         (gdocs-sync--handle-push-error err))))))

(defun gdocs-sync--handle-push-success (current-ir response)
  "Update sync state after a successful push.
CURRENT-IR is the IR that was pushed.  RESPONSE is the API
response alist."
  (setq gdocs-sync--shadow-ir current-ir)
  (gdocs-sync--update-revision-from-response response)
  (gdocs-sync--update-last-sync-time)
  (gdocs-sync--set-status 'synced)
  (setq gdocs-sync--push-in-progress nil)
  (gdocs-sync--drain-push-queue)
  (message "Pushed to Google Docs"))

(defun gdocs-sync--handle-push-error (err)
  "Handle a push failure.
ERR is the error that occurred."
  (gdocs-sync--set-status 'error)
  (setq gdocs-sync--push-in-progress nil)
  (message "Push failed: %s" (error-message-string err)))

(defun gdocs-sync--make-push-error-callback (buf)
  "Return an error callback for a push API request.
BUF is the originating buffer.  Resets push state so subsequent
pushes are not blocked."
  (lambda (err)
    (with-current-buffer buf
      (gdocs-sync--handle-push-error err))))

(defun gdocs-sync--drain-push-queue ()
  "If a push is queued, trigger it now."
  (when gdocs-sync--push-queued
    (setq gdocs-sync--push-queued nil)
    (gdocs-sync-push)))

(defun gdocs-sync--update-revision-from-response (response)
  "Extract and store the revision ID from RESPONSE."
  (when-let* ((write-control (alist-get 'writeControl response))
              (rev-id (alist-get 'requiredRevisionId write-control)))
    (setq gdocs-sync--revision-id rev-id)))

(defun gdocs-sync--update-last-sync-time ()
  "Set the last sync time to the current time in ISO 8601 format."
  (setq gdocs-sync--last-sync-time (format-time-string "%FT%T%z")))

;;;; Pull

(defun gdocs-sync-pull ()
  "Pull remote changes from the linked Google Doc."
  (interactive)
  (unless gdocs-sync--document-id
    (user-error "Buffer is not linked to a Google Doc"))
  (let ((buf (current-buffer)))
    (gdocs-api-get-file-metadata
     gdocs-sync--document-id
     (lambda (metadata)
       (with-current-buffer buf
         (gdocs-sync--handle-metadata-for-pull metadata)))
     gdocs-sync--account)))

(defun gdocs-sync--handle-metadata-for-pull (metadata)
  "Check revision in METADATA and fetch document if changed."
  (let ((remote-rev (alist-get 'headRevisionId metadata)))
    (if (gdocs-sync--revision-matches-p remote-rev)
        (message "Already up to date.")
      (gdocs-sync--fetch-document-for-pull))))

(defun gdocs-sync--revision-matches-p (remote-rev)
  "Return non-nil if REMOTE-REV matches the stored revision."
  (and gdocs-sync--revision-id
       (equal gdocs-sync--revision-id remote-rev)))

(defun gdocs-sync--fetch-document-for-pull ()
  "Fetch the full document for a pull operation."
  (let ((buf (current-buffer)))
    (gdocs-api-get-document
     gdocs-sync--document-id
     (lambda (json)
       (with-current-buffer buf
         (gdocs-sync--apply-pull json)))
     gdocs-sync--account)))

(defun gdocs-sync--apply-pull (json)
  "Apply the pulled document JSON to the current buffer."
  (let* ((remote-ir (gdocs-convert-docs-json-to-ir json))
         (remote-org (gdocs-convert-ir-to-org remote-ir)))
    (if (gdocs-sync--has-local-modifications-p)
        (gdocs-sync--start-conflict-resolution remote-org)
      (gdocs-sync--replace-buffer-content remote-org remote-ir))))

(defun gdocs-sync--has-local-modifications-p ()
  "Return non-nil if the buffer has local modifications.
A buffer is considered modified if it is marked as modified by
Emacs or if the current IR differs from the shadow IR."
  (or (buffer-modified-p)
      (gdocs-sync--content-differs-from-shadow-p)))

(defun gdocs-sync--content-differs-from-shadow-p ()
  "Return non-nil if the buffer content differs from the shadow IR."
  (when gdocs-sync--shadow-ir
    (let ((current-ir (gdocs-convert-org-buffer-to-ir)))
      (not (equal current-ir gdocs-sync--shadow-ir)))))

(defun gdocs-sync--replace-buffer-content (org-string remote-ir)
  "Replace buffer content with ORG-STRING and update shadow to REMOTE-IR."
  (let ((inhibit-modification-hooks t))
    (erase-buffer)
    (insert org-string)
    (set-buffer-modified-p nil))
  (setq gdocs-sync--shadow-ir remote-ir)
  (gdocs-sync--update-last-sync-time)
  (gdocs-sync--set-status 'synced)
  (message "Pulled remote changes."))

(defun gdocs-sync--start-conflict-resolution (remote-org)
  "Begin merge resolution between local content and REMOTE-ORG."
  (require 'gdocs-merge)
  (gdocs-sync--set-status 'conflict)
  (let ((local-org (buffer-substring-no-properties (point-min) (point-max)))
        (buf (current-buffer)))
    (gdocs-merge-start
     local-org
     remote-org
     (lambda (merged-org)
       (with-current-buffer buf
         (gdocs-sync--apply-merge-result merged-org))))))

(defun gdocs-sync--apply-merge-result (merged-org)
  "Apply MERGED-ORG as the resolved content and push."
  (let ((inhibit-modification-hooks t))
    (erase-buffer)
    (insert merged-org))
  (setq gdocs-sync--shadow-ir (gdocs-convert-org-buffer-to-ir))
  (gdocs-sync--update-last-sync-time)
  (gdocs-sync--set-status 'synced)
  (save-buffer)
  (message "Merge complete."))

;;;; Push on save

(defun gdocs-sync--push-on-save ()
  "Push to Google Docs after saving, if appropriate.
Intended for use in `after-save-hook'."
  (when (gdocs-sync--should-auto-push-p)
    (gdocs-sync-push)))

(defun gdocs-sync--should-auto-push-p ()
  "Return non-nil if an automatic push should proceed."
  (and gdocs-auto-push-on-save
       gdocs-sync--document-id
       (not (memq gdocs-sync--status '(conflict error)))))

;;;; Link and unlink

(defun gdocs-sync-link (document-id-or-url &optional account file-path)
  "Link the current buffer to a Google Doc.
DOCUMENT-ID-OR-URL is a document ID or full Google Docs URL.
ACCOUNT is the account name to use; if nil, prompt.  FILE-PATH
is unused but accepted for API compatibility."
  (interactive
   (progn
     (gdocs-auth--validate-accounts-configured)
     (list (read-string "Google Doc ID or URL: "))))
  (ignore file-path)
  (let ((doc-id (gdocs-sync--parse-document-id document-id-or-url))
        (acct (or account (gdocs-auth-select-account "Account: "))))
    (gdocs-sync--write-file-local-vars doc-id acct)
    (setq gdocs-sync--document-id doc-id)
    (setq gdocs-sync--account acct)
    (save-buffer)
    (gdocs-sync-pull)))

(defun gdocs-sync-unlink ()
  "Remove Google Docs link from the current buffer."
  (interactive)
  (gdocs-sync--remove-file-local-vars)
  (gdocs-sync--clear-buffer-state)
  (save-buffer)
  (message "Unlinked from Google Docs."))

(defun gdocs-sync--write-file-local-vars (doc-id account)
  "Write file-local variables for DOC-ID and ACCOUNT."
  (add-file-local-variable 'gdocs-document-id doc-id)
  (add-file-local-variable 'gdocs-account account))

(defun gdocs-sync--remove-file-local-vars ()
  "Remove all gdocs file-local variables."
  (delete-file-local-variable 'gdocs-document-id)
  (delete-file-local-variable 'gdocs-account)
  (delete-file-local-variable 'gdocs-revision-id)
  (delete-file-local-variable 'gdocs-last-sync))

(defun gdocs-sync--clear-buffer-state ()
  "Clear all buffer-local sync state."
  (setq gdocs-sync--document-id nil)
  (setq gdocs-sync--account nil)
  (setq gdocs-sync--shadow-ir nil)
  (setq gdocs-sync--revision-id nil)
  (setq gdocs-sync--last-sync-time nil)
  (setq gdocs-sync--push-in-progress nil)
  (setq gdocs-sync--push-queued nil)
  (gdocs-sync--set-status 'synced))

;;;; URL parsing

(defconst gdocs-sync--url-regexp
  "https://docs\\.google\\.com/document/d/\\([^/]+\\)"
  "Regexp matching a Google Docs URL, capturing the document ID.")

(defun gdocs-sync--parse-document-id (id-or-url)
  "Extract document ID from ID-OR-URL.
If ID-OR-URL is a Google Docs URL, extract the document ID.
Otherwise return ID-OR-URL as-is."
  (if (string-match gdocs-sync--url-regexp id-or-url)
      (match-string 1 id-or-url)
    id-or-url))

;;;; Status management

(defun gdocs-sync--set-status (status)
  "Set the sync status to STATUS and update the modeline."
  (setq gdocs-sync--status status)
  (when (fboundp 'gdocs--update-modeline)
    (gdocs--update-modeline)))

;;;; State initialization from file-local variables

(defun gdocs-sync--init-from-file-locals ()
  "Initialize buffer-local sync state from file-local variables."
  (when (bound-and-true-p gdocs-document-id)
    (setq gdocs-sync--document-id gdocs-document-id))
  (when (bound-and-true-p gdocs-account)
    (setq gdocs-sync--account gdocs-account))
  (when (bound-and-true-p gdocs-revision-id)
    (setq gdocs-sync--revision-id gdocs-revision-id))
  (when (bound-and-true-p gdocs-last-sync)
    (setq gdocs-sync--last-sync-time gdocs-last-sync)))

;;;; Title filtering

(defun gdocs-sync--body-start-index (ir)
  "Compute the UTF-16 index where non-title body content starts.
Title elements occupy space in the Google Doc but are filtered
from the IR before diffing.  This function returns the index
after all title elements, so that the diff engine generates
correct document indices."
  (let ((offset 0))
    (dolist (element ir)
      (when (eq (plist-get element :style) 'title)
        (setq offset (+ offset (gdocs-diff--element-utf16-length element)))))
    (+ 1 offset)))

(defun gdocs-sync--filter-title (ir)
  "Return IR with title elements removed.
The document title comes from metadata, not the body.  Including
it in the IR causes index misalignment during diff and push."
  (cl-remove-if (lambda (element)
                  (eq (plist-get element :style) 'title))
                ir))

(provide 'gdocs-sync)
;;; gdocs-sync.el ends here
