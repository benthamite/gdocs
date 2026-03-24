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
(require 'gdocs-merge)
(declare-function gdocs--update-modeline "gdocs")
(declare-function gdocs--ensure-org-tag "gdocs")
(declare-function gdocs--remove-org-tag "gdocs")

;;;; Customizable variables

(defcustom gdocs-directory (expand-file-name "~/org/gdocs/")
  "Default directory for synced org files."
  :type 'directory
  :group 'gdocs)

(defcustom gdocs-auto-push-on-save t
  "Whether to automatically push changes on save."
  :type 'boolean
  :group 'gdocs)

(defcustom gdocs-preserve-comments t
  "When non-nil, check for Google Docs comments before pushing.
Destructive changes (element deletions, cross-type modifications)
that would destroy comments prompt for per-element confirmation."
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

;;;; Link context

(defun gdocs-sync--make-link-context ()
  "Build a link context plist for cross-document resolution."
  (list :buffer-file buffer-file-name
        :docid-map (gdocs-convert--build-docid-to-file-map
                    (cons gdocs-directory
                          (bound-and-true-p gdocs-link-directories)))))

;;;; Push

(defun gdocs-sync-push ()
  "Push local changes to the linked Google Doc.
Fetches the remote document first to populate the heading cache,
ensuring same-document heading links resolve to anchored URLs."
  (interactive)
  (unless gdocs-sync--document-id
    (user-error "Buffer is not linked to a Google Doc"))
  (unless (gdocs-sync--serialize-push)
    (setq gdocs-sync--push-in-progress t)
    (gdocs-sync--set-status 'pushing)
    (let ((buf (current-buffer))
          (doc-id gdocs-sync--document-id)
          (acct gdocs-sync--account))
      (gdocs-api-get-document
       doc-id
       (lambda (json)
         (condition-case err
             (with-current-buffer buf
               (gdocs-convert--cache-heading-ids doc-id json)
               (let* ((link-ctx (gdocs-sync--make-link-context))
                      (gdocs-convert--link-context link-ctx)
                      (current-ir (gdocs-convert-org-buffer-to-ir)))
                 (gdocs-sync--push-incremental
                  current-ir buf link-ctx json)))
           ((error quit)
            (if (buffer-live-p buf)
                (with-current-buffer buf
                  (gdocs-sync--handle-push-error err))
              (message "Push failed: %s"
                       (error-message-string err))))))
       acct
       (gdocs-sync--make-push-error-callback buf)))))

(defun gdocs-sync--serialize-push ()
  "If a push is already in progress, queue this one.
Return non-nil if the push was serialized (caller should abort)."
  (when gdocs-sync--push-in-progress
    (setq gdocs-sync--push-queued t)
    (message "Push already in progress, queued")
    t))


(defun gdocs-sync--push-incremental (current-ir buf link-ctx json)
  "Push CURRENT-IR as an incremental diff against the remote document.
BUF is the originating buffer.  LINK-CTX is the link context for
re-binding in callbacks.  JSON is the pre-fetched document used
for accurate UTF-16 indices."
  (let* ((doc-id gdocs-sync--document-id)
         (acct gdocs-sync--account)
         (local-ir current-ir)
         (remote-ir (gdocs-convert-docs-json-to-ir json))
         (remote-title (alist-get 'title json))
         (start-index (gdocs-sync--body-start-index remote-ir))
         (remote-filtered (gdocs-sync--filter-empty-paragraphs
                           (gdocs-sync--filter-title remote-ir)))
         (local-filtered (gdocs-sync--filter-title local-ir)))
    (gdocs-sync--maybe-rename-document
     local-ir remote-title doc-id acct)
    (if gdocs-preserve-comments
        ;; Fetch comments, then push with comment protection
        (gdocs-api-list-comments
         doc-id
         (lambda (comments)
           (when (buffer-live-p buf)
             (with-current-buffer buf
               (let ((gdocs-convert--link-context link-ctx))
                 (gdocs-sync--push-with-comment-protection
                  local-ir local-filtered remote-filtered
                  comments start-index buf link-ctx)))))
         acct)
      ;; Standard flow: compute diff and push
      (let ((requests (gdocs-diff-generate
                       remote-filtered local-filtered start-index)))
        (if (null requests)
            (gdocs-sync--push-no-changes buf)
          (gdocs-api-batch-update
           doc-id requests
           (gdocs-sync--make-push-callback local-ir buf link-ctx)
           acct
           (gdocs-sync--make-push-error-callback buf)))))))

(defun gdocs-sync--push-no-changes (buf)
  "Handle the case where push found no changes.
BUF is the originating buffer."
  (with-current-buffer buf
    (setq gdocs-sync--push-in-progress nil)
    (gdocs-sync--set-status 'synced)
    (message "No changes to push")))

(defun gdocs-sync--make-push-callback (current-ir buf link-ctx)
  "Return a callback for a push API response.
CURRENT-IR is the IR that was pushed.  BUF is the originating
buffer.  LINK-CTX is the link context for re-binding when setting
the shadow."
  (lambda (response)
    (if (not (buffer-live-p buf))
        (progn
          (message "gdocs: push completed but buffer was killed")
          ;; Release the lock even if the buffer is gone
          (ignore-errors
            (with-current-buffer buf
              (setq gdocs-sync--push-in-progress nil))))
      (condition-case err
          (with-current-buffer buf
            (let ((gdocs-convert--link-context link-ctx))
              (gdocs-sync--handle-push-success current-ir response)))
        ((error quit)
         (if (buffer-live-p buf)
             (with-current-buffer buf
               (gdocs-sync--handle-push-error err))
           (message "Push failed: %s" (error-message-string err))))))))

(defun gdocs-sync--handle-push-success (current-ir response)
  "Update sync state after a successful push.
CURRENT-IR is the IR that was pushed.  RESPONSE is the API
response alist.  When the pushed IR contains nested list items
\(level > 0), triggers a post-push fixup to correct nesting
before finalizing."
  (setq gdocs-sync--shadow-ir current-ir)
  (gdocs-sync--update-revision-from-response response)
  (gdocs-sync--update-last-sync-time)
  (gdocs-sync--persist-properties)
  (when buffer-file-name
    (let ((gdocs-auto-push-on-save nil)
          (before-save-hook nil)
          (after-save-hook nil))
      (save-buffer)))
  (if (gdocs-sync--ir-has-nested-lists-p current-ir)
      ;; Nested lists need a fixup pass — keep push-in-progress
      ;; until the fixup completes.
      (gdocs-sync--begin-list-nesting-fixup current-ir)
    (gdocs-sync--finalize-push)))

(defun gdocs-sync--finalize-push ()
  "Complete the push cycle: set status, release lock, drain queue."
  (gdocs-sync--set-status 'synced)
  (setq gdocs-sync--push-in-progress nil)
  (gdocs-sync--drain-push-queue)
  (message "Pushed to Google Docs"))

;;;; Post-push list nesting fixup
;;
;; The diff path emits per-paragraph `createParagraphBullets' requests,
;; which creates flat (level-0) lists.  The full conversion path has
;; `gdocs-convert--fixup-list-nesting' to group bullets, but the diff
;; path cannot use that mechanism because it interleaves modifications
;; with insertions at varying document positions.
;;
;; This fixup runs a second batchUpdate after the main push succeeds.
;; It fetches the just-pushed document to get current paragraph ranges,
;; then for each contiguous list group containing nested items:
;;   1. Inserts tab characters at each nested paragraph's startIndex
;;   2. Deletes all existing bullets in the group range
;;   3. Creates bullets for the entire group as a single request
;; The `createParagraphBullets' API reads the tabs, assigns correct
;; nesting levels, and removes the tabs.

(defun gdocs-sync--ir-has-nested-lists-p (ir)
  "Return non-nil if IR contains any list items with level > 0."
  (cl-some (lambda (elem)
             (let ((list-info (plist-get elem :list)))
               (and list-info
                    (> (or (plist-get list-info :level) 0) 0))))
           ir))

(defun gdocs-sync--begin-list-nesting-fixup (local-ir)
  "Fetch the pushed document and apply list nesting fixup.
LOCAL-IR is the IR that was just pushed (with correct :level values)."
  (let ((buf (current-buffer))
        (doc-id gdocs-sync--document-id)
        (acct gdocs-sync--account)
        (ir local-ir))
    (message "Fixing list nesting...")
    (gdocs-api-get-document
     doc-id
     (lambda (json)
       (if (not (buffer-live-p buf))
           (message "gdocs: fixup completed but buffer was killed")
         (condition-case err
             (with-current-buffer buf
               (gdocs-sync--apply-list-nesting-fixup ir json))
           ((error quit)
            (if (buffer-live-p buf)
                (with-current-buffer buf
                  (gdocs-sync--handle-push-error err))
              (message "List nesting fixup failed: %s"
                       (error-message-string err)))))))
     acct
     (gdocs-sync--make-push-error-callback buf))))

(defun gdocs-sync--apply-list-nesting-fixup (local-ir json)
  "Compute and send the list nesting fixup batchUpdate.
LOCAL-IR has the correct :level for each element.  JSON is the
freshly fetched document with current paragraph ranges."
  (let* ((doc-id gdocs-sync--document-id)
         (acct gdocs-sync--account)
         (buf (current-buffer))
         (remote-ir (gdocs-convert-docs-json-to-ir json))
         (remote-filtered (gdocs-sync--filter-empty-paragraphs
                           (gdocs-sync--filter-title remote-ir)))
         (local-filtered (gdocs-sync--filter-title local-ir))
         (requests (gdocs-sync--compute-nesting-fixup-requests
                    local-filtered remote-filtered)))
    (if (null requests)
        (progn
          (message "No list nesting fixup needed")
          (gdocs-sync--finalize-push))
      (gdocs-api-batch-update
       doc-id requests
       (lambda (response)
         (if (not (buffer-live-p buf))
             (message "gdocs: fixup completed but buffer was killed")
           (condition-case err
               (with-current-buffer buf
                 (gdocs-sync--update-revision-from-response response)
                 (gdocs-sync--persist-properties)
                 (gdocs-sync--finalize-push))
             ((error quit)
              (if (buffer-live-p buf)
                  (with-current-buffer buf
                    (gdocs-sync--handle-push-error err))
                (message "List nesting fixup failed: %s"
                         (error-message-string err)))))))
       acct
       (gdocs-sync--make-push-error-callback buf)))))

(defun gdocs-sync--compute-nesting-fixup-requests (local-ir remote-ir)
  "Compute batchUpdate requests to fix list nesting.
LOCAL-IR has the correct :level from the org buffer.  REMOTE-IR
has :doc-start and :doc-end from the fetched document.

Aligns elements by text content, then for each contiguous list
group containing elements at level > 0, deletes existing bullets,
resets indentation to zero, inserts leading tab characters (N tabs
for level N), and re-creates bullets as a single grouped request.
The Google Docs API counts leading tabs to determine nesting level
and removes them afterward.

Returns the ordered request list, or nil if no fixup is needed."
  (let* ((aligned (gdocs-sync--align-ir-elements local-ir remote-ir))
         (element-ranges (gdocs-sync--build-fixup-element-ranges aligned))
         (groups (gdocs-convert--find-list-groups element-ranges))
         (groups-needing-fixup
          (cl-remove-if-not
           (lambda (group)
             (gdocs-sync--group-needs-nesting-fixup-p group element-ranges))
           groups)))
    (when groups-needing-fixup
      (gdocs-convert--alternate-numbered-presets groups-needing-fixup)
      (let ((delete-reqs nil)
            (reset-reqs nil)
            (tab-reqs nil))
        (dolist (group groups-needing-fixup)
          (let ((group-start (plist-get group :start))
                (group-end (plist-get group :end)))
            ;; Delete bullets for the group range
            (push `((deleteParagraphBullets
                     . ((range . ((startIndex . ,group-start)
                                  (endIndex . ,(1- group-end)))))))
                  delete-reqs)
            ;; Reset indentation to zero and collect tab insertions.
            (dolist (er element-ranges)
              (let* ((elem (plist-get er :element))
                     (start (plist-get er :start))
                     (end (plist-get er :end))
                     (list-info (plist-get elem :list))
                     (level (if list-info
                                (or (plist-get list-info :level) 0)
                              0)))
                (when (and (>= start group-start)
                           (<= end group-end))
                  ;; Reset ALL indentation to zero so only tabs
                  ;; signal the nesting depth.
                  (push `((updateParagraphStyle
                           . ((paragraphStyle
                               . ((indentStart . ((magnitude . 0)
                                                  (unit . "PT")))
                                  (indentFirstLine . ((magnitude . 0)
                                                      (unit . "PT")))))
                              (range . ((startIndex . ,start)
                                        (endIndex . ,(1- end))))
                              (fields . "indentStart,indentFirstLine"))))
                        reset-reqs)
                  ;; Insert leading tabs for nested items
                  (when (> level 0)
                    (push (cons start level) tab-reqs)))))))
        ;; Sort tab insertions by descending index
        (setq tab-reqs
              (sort tab-reqs (lambda (a b) (> (car a) (car b)))))
        ;; Merge all groups into one createParagraphBullets so that
        ;; mixed-type nested lists share one list object with correct
        ;; nesting levels determined by leading tabs.
        (let ((merged-start (plist-get (car groups-needing-fixup) :start))
              (merged-end (plist-get (car (last groups-needing-fixup)) :end))
              (merged-preset (plist-get (car groups-needing-fixup) :preset)))
          (append
           ;; 1. Delete existing bullets
           (nreverse delete-reqs)
           ;; 2. Reset indentation to zero
           (nreverse reset-reqs)
           ;; 3. Insert leading tabs (descending order)
           (mapcar (lambda (tr)
                     (gdocs-convert--make-insert-text-request
                      (make-string (cdr tr) ?\t)
                      (car tr)))
                   tab-reqs)
           ;; 4. Create ONE grouped bullet
           (list `((createParagraphBullets
                    . ((range . ((startIndex . ,merged-start)
                                 (endIndex . ,(1- merged-end))))
                       (bulletPreset . ,merged-preset)))))))))))


(defun gdocs-sync--align-ir-elements (local-ir remote-ir)
  "Align LOCAL-IR and REMOTE-IR elements by text content.
Returns a list of plists, each with :local (local IR element or nil)
and :remote (remote IR element or nil), matched by plain text content.
Uses the same LCS approach as the diff engine."
  (let* ((local-texts (mapcar #'gdocs-sync--element-plain-text local-ir))
         (remote-texts (mapcar #'gdocs-sync--element-plain-text remote-ir))
         (lcs-pairs (gdocs-diff--lcs local-texts remote-texts))
         (local-matched (make-hash-table :test 'eql))
         (remote-matched (make-hash-table :test 'eql))
         (result nil))
    ;; Mark matched pairs
    (dolist (pair lcs-pairs)
      (puthash (car pair) (cdr pair) local-matched)
      (puthash (cdr pair) (car pair) remote-matched))
    ;; Build aligned list: walk remote IR, include matched locals
    (let ((ri 0))
      (dolist (remote-elem remote-ir)
        (let ((li (gethash ri remote-matched)))
          (push (list :local (when li (nth li local-ir))
                      :remote remote-elem)
                result))
        (setq ri (1+ ri))))
    (nreverse result)))

(defun gdocs-sync--build-fixup-element-ranges (aligned)
  "Build element-range plists from ALIGNED pairs for fixup.
Each entry has :element (using local level info merged into remote
range info), :start, and :end.  The :element carries the local
:list info (for correct :level) when available, falling back to
the remote element's list info."
  (let ((result nil))
    (dolist (pair aligned)
      (let* ((local-elem (plist-get pair :local))
             (remote-elem (plist-get pair :remote))
             (start (plist-get remote-elem :doc-start))
             (end (plist-get remote-elem :doc-end)))
        (when (and start end)
          ;; Build a synthetic element with the remote's structural info
          ;; but the local's :list (which has the correct :level).
          (let ((elem (copy-sequence remote-elem)))
            (when (and local-elem (plist-get local-elem :list))
              (plist-put elem :list (plist-get local-elem :list)))
            (push (list :element elem :start start :end end) result)))))
    (nreverse result)))

(defun gdocs-sync--group-needs-nesting-fixup-p (group element-ranges)
  "Return non-nil if GROUP contains elements needing nesting fixup.
GROUP is a plist with :start and :end document indices.
Checks whether any list element in the group has level > 0."
  (let ((group-start (plist-get group :start))
        (group-end (plist-get group :end)))
    (cl-some
     (lambda (er)
       (let* ((elem (plist-get er :element))
              (start (plist-get er :start))
              (end (plist-get er :end))
              (list-info (plist-get elem :list))
              (level (if list-info
                         (or (plist-get list-info :level) 0)
                       0)))
         (and (>= start group-start)
              (<= end group-end)
              (> level 0))))
     element-ranges)))

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
    (if (buffer-live-p buf)
        (with-current-buffer buf
          (gdocs-sync--handle-push-error err))
      (message "Push failed: %s" (error-message-string err)))))

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
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (gdocs-sync--handle-metadata-for-pull metadata))))
     gdocs-sync--account)))

(defun gdocs-sync--handle-metadata-for-pull (metadata)
  "Check revision in METADATA and fetch document if changed."
  (let ((remote-rev (alist-get 'headRevisionId metadata)))
    (if (gdocs-sync--revision-matches-p remote-rev)
        (message "Already up to date.")
      (gdocs-sync--fetch-document-for-pull remote-rev))))

(defun gdocs-sync--revision-matches-p (remote-rev)
  "Return non-nil if REMOTE-REV matches the stored revision."
  (and gdocs-sync--revision-id
       (equal gdocs-sync--revision-id remote-rev)))

(defun gdocs-sync--fetch-document-for-pull (revision-id)
  "Fetch the full document for a pull operation.
REVISION-ID is the remote revision to store on success."
  (let ((buf (current-buffer))
        (rev revision-id))
    (gdocs-api-get-document
     gdocs-sync--document-id
     (lambda (json)
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (gdocs-sync--apply-pull json rev))))
     gdocs-sync--account)))

(defun gdocs-sync--apply-pull (json revision-id)
  "Apply the pulled document JSON to the current buffer.
REVISION-ID is the remote revision to store on success.
Uses the shadow IR to detect what actually changed remotely.
When a shadow exists, performs a three-way merge that preserves
org-only metadata (property drawers, TODO keywords, tags, etc.)
on elements that were not changed remotely."
  (let* ((doc-id gdocs-sync--document-id)
         (link-ctx (gdocs-sync--make-link-context))
         (gdocs-convert--document-id doc-id)
         (remote-ir (gdocs-convert-docs-json-to-ir json)))
    (when doc-id
      (gdocs-convert--cache-heading-ids doc-id json))
    (if (gdocs-sync--remote-unchanged-p remote-ir)
        (progn
          (when revision-id
            (setq gdocs-sync--revision-id revision-id))
          (gdocs-sync--set-status 'synced)
          (message "Already up to date."))
      (let ((gdocs-convert--link-context link-ctx))
        (if (null gdocs-sync--shadow-ir)
            ;; No shadow (first pull after link): full replacement
            (gdocs-sync--replace-buffer-content
             (gdocs-convert-ir-to-org remote-ir) revision-id)
          ;; Have shadow: three-way merge
          (let ((result (gdocs-sync--three-way-merge remote-ir)))
            (if (plist-get result :has-conflicts)
                (gdocs-sync--start-conflict-resolution
                 (plist-get result :merged-org) revision-id)
              (gdocs-sync--replace-buffer-content
               (plist-get result :merged-org) revision-id))))))))

(defun gdocs-sync--remote-unchanged-p (remote-ir)
  "Return non-nil if REMOTE-IR matches the shadow IR.
Compares content-based element keys rather than structural
equality, so non-content fields like element IDs and org-only
markers do not cause false mismatches."
  (when gdocs-sync--shadow-ir
    (equal (gdocs-diff--element-keys (gdocs-sync--filter-title remote-ir))
           (gdocs-diff--element-keys
            (gdocs-sync--filter-title gdocs-sync--shadow-ir)))))

(defun gdocs-sync--install-content (org-string &optional revision-id)
  "Replace buffer content with ORG-STRING and update sync state.
REVISION-ID, if non-nil, is stored as the current revision.
Handles suppressing modification hooks, writing properties,
resetting org-element cache, and setting the shadow IR from the
buffer."
  ;; Suppress modification hooks to prevent recursive push-on-save
  (let ((inhibit-modification-hooks t)
        (doc-id gdocs-sync--document-id)
        (saved-point (point)))
    (erase-buffer)
    (insert org-string)
    (gdocs--ensure-org-tag)
    ;; Update buffer-local state before writing properties so
    ;; persist-properties sees the current values.
    (when revision-id
      (setq gdocs-sync--revision-id revision-id))
    (gdocs-sync--update-last-sync-time)
    ;; Write all metadata to the file-level property drawer.
    (when doc-id
      (gdocs-sync--persist-properties))
    (goto-char (min saved-point (point-max))))
  ;; Persist content to disk so it survives buffer kill.
  ;; Suppress hooks: org-entry-put invalidated the org-element cache,
  ;; and expensive before-save-hook entries (e.g. org-encrypt-entries)
  ;; would have to rebuild it from scratch.
  (when buffer-file-name
    (let ((gdocs-auto-push-on-save nil)
          (before-save-hook nil)
          (after-save-hook nil))
      (save-buffer)))
  ;; The cache is stale after a full buffer replacement with
  ;; `inhibit-modification-hooks' bound — reset it to avoid
  ;; org-element parser errors.
  (when (derived-mode-p 'org-mode)
    (org-element-cache-reset))
  ;; Set the shadow from the buffer so it matches what
  ;; `gdocs-convert-org-buffer-to-ir' produces on the next push.
  ;; Using the raw remote-ir would cause representation mismatches
  ;; (empty paragraphs, run structure differences) that produce
  ;; phantom diff operations.
  (setq gdocs-sync--shadow-ir (gdocs-convert-org-buffer-to-ir))
  (gdocs-sync--set-status 'synced))

(defun gdocs-sync--replace-buffer-content (org-string &optional revision-id)
  "Replace buffer content with ORG-STRING and update the shadow IR.
REVISION-ID, if non-nil, is stored as the current revision."
  (gdocs-sync--install-content org-string revision-id)
  (message "Pulled remote changes."))

(defun gdocs-sync--start-conflict-resolution (remote-org revision-id)
  "Begin merge resolution between local content and REMOTE-ORG.
REVISION-ID is the remote revision to store after resolution."
  (require 'gdocs-merge)
  (gdocs-sync--set-status 'conflict)
  (let ((local-org (buffer-substring-no-properties (point-min) (point-max)))
        (buf (current-buffer))
        (link-ctx gdocs-convert--link-context)
        (rev revision-id))
    (gdocs-merge-start
     local-org
     remote-org
     (lambda (merged-org)
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (let ((gdocs-convert--link-context link-ctx))
             (gdocs-sync--apply-merge-result merged-org rev))))))))

(defun gdocs-sync--apply-merge-result (merged-org &optional revision-id)
  "Apply MERGED-ORG as the resolved content and push.
REVISION-ID, if non-nil, is stored as the current revision."
  (gdocs-sync--install-content merged-org revision-id)
  (message "Merge complete."))

;;;; Three-way merge

(defun gdocs-sync--three-way-merge (remote-ir)
  "Merge REMOTE-IR into the current buffer using three-way merge.
Uses the shadow IR as the common ancestor.  For elements
unchanged remotely, preserves the local org text (including
property drawers and other org-only metadata).  For elements
changed remotely, uses the remote version with local markers
grafted back.  Returns a plist with :merged-org and
:has-conflicts."
  ;; Prepare three-way data and compute LCS
  (let* ((shadow-ir (gdocs-sync--filter-title gdocs-sync--shadow-ir))
         (remote-filtered (gdocs-sync--filter-title remote-ir))
         (local-data (gdocs-convert-org-buffer-to-segments))
         (local-ir (gdocs-sync--filter-title (plist-get local-data :ir)))
         (local-segments (plist-get local-data :segments))
         (preamble (plist-get local-data :preamble))
         (postamble (plist-get local-data :postamble))
         ;; Compute element keys for all three
         (shadow-keys (gdocs-diff--element-keys shadow-ir))
         (remote-keys (gdocs-diff--element-keys remote-filtered))
         (local-keys (gdocs-diff--element-keys local-ir))
         ;; LCS: remote vs shadow (what changed remotely)
         (rs-lcs (gdocs-diff--lcs shadow-keys remote-keys))
         (rs-ops (gdocs-diff--classify-operations
                  shadow-ir remote-filtered rs-lcs))
         ;; LCS: local vs shadow (what changed locally)
         (ls-lcs (gdocs-diff--lcs shadow-keys local-keys))
         (ls-ops (gdocs-diff--classify-operations
                  shadow-ir local-ir ls-lcs))
         ;; Map: shadow-index -> local-segment-index (unchanged elements)
         ;; ls-lcs pairs only elements with equal keys.
         (shadow-to-local ls-lcs)
         ;; Map: shadow-index -> local-segment-index (modified elements)
         ;; Built from :modify operations where the content key changed
         ;; but positional correspondence was detected by the LCS
         ;; algorithm's adjacent delete+insert collapsing.
         (shadow-to-local-modified
          (cl-loop for op in ls-ops
                   when (eq (plist-get op :op) 'modify)
                   collect (cons (plist-get op :old-index)
                                 (plist-get op :new-index))))
         ;; Merge
         (merged-parts nil)
         (has-conflicts nil))
    ;; Walk the remote operations to build the merged output
    (dolist (op rs-ops)
      (let ((op-type (plist-get op :op)))
        (pcase op-type
          ('keep
           ;; Remote unchanged: use local text if available
           (let* ((si (plist-get op :old-index))
                  (local-idx (cdr (assq si shadow-to-local)))
                  (modified-idx (cdr (assq si shadow-to-local-modified))))
             (cond
              (local-idx
               ;; LCS match: local kept this element, use it verbatim
               (push (plist-get (nth local-idx local-segments) :org-text)
                     merged-parts))
              (modified-idx
               ;; Local modified this element, remote kept it:
               ;; preserve the local version
               (push (plist-get (nth modified-idx local-segments) :org-text)
                     merged-parts))
              (t
               ;; No local match (local deleted this element):
               ;; remote kept it, so restore from remote
               (push (gdocs-sync--ir-element-to-org-segment
                      (nth (plist-get op :new-index) remote-filtered)
                      nil)
                     merged-parts)))))
          ('insert
           ;; Remote inserted new element
           (push (gdocs-sync--ir-element-to-org-segment
                  (nth (plist-get op :new-index) remote-filtered)
                  nil)
                 merged-parts))
          ('delete
           ;; Remote deleted: check if local also changed it
           (let* ((si (plist-get op :old-index))
                  (modified-idx (cdr (assq si shadow-to-local-modified))))
             (cond
              (modified-idx
               ;; Local modified what remote deleted: conflict
               (setq has-conflicts t)
               (push (plist-get (nth modified-idx local-segments) :org-text)
                     merged-parts))
              (t
               ;; Local unchanged or also deleted: honor remote deletion
               nil))))
          ('modify
           ;; Remote changed this element
           (let* ((si (plist-get op :old-index))
                  (ni (plist-get op :new-index))
                  (remote-elem (nth ni remote-filtered))
                  (local-idx (cdr (assq si shadow-to-local)))
                  (modified-idx (cdr (assq si shadow-to-local-modified))))
             (if modified-idx
                 ;; Both changed: conflict — include both versions
                 (progn
                   (setq has-conflicts t)
                   (push (concat
                          gdocs-merge-conflict-marker-local
                          (plist-get (nth modified-idx local-segments)
                                     :org-text)
                          gdocs-merge-conflict-marker-separator
                          (gdocs-sync--ir-element-to-org-segment
                           remote-elem nil)
                          gdocs-merge-conflict-marker-remote)
                         merged-parts))
               ;; Only remote changed: use remote, graft local metadata
               (let* ((local-seg (when local-idx
                                   (nth local-idx local-segments)))
                      (drawer (when local-seg
                                (gdocs-sync--extract-property-drawer
                                 (plist-get local-seg :org-text))))
                      (local-elem (when local-idx
                                    (nth local-idx local-ir)))
                      (grafted (gdocs-sync--graft-markers
                                remote-elem local-elem)))
                 (push (gdocs-sync--ir-element-to-org-segment
                        grafted drawer)
                       merged-parts))))))))
    ;; Assemble final result
    (list :merged-org (concat preamble
                              (apply #'concat (nreverse merged-parts))
                              postamble)
          :has-conflicts has-conflicts)))

(defun gdocs-sync--ir-element-to-org-segment (element drawer)
  "Convert an IR ELEMENT to an org text segment.
If DRAWER is non-nil, graft the property drawer string after the
heading line.  Includes a trailing newline."
  (let ((org-text (gdocs-convert--ir-element-to-org element)))
    (if (and drawer (gdocs-sync--heading-element-p element))
        (concat (gdocs-sync--graft-drawer-into-heading org-text drawer)
                "\n")
      (concat org-text "\n"))))

(defun gdocs-sync--heading-element-p (element)
  "Return non-nil if ELEMENT is a heading."
  (and (eq (plist-get element :type) 'paragraph)
       (let ((style (plist-get element :style)))
         (and style
              (string-prefix-p "heading-" (symbol-name style))))))

(defun gdocs-sync--extract-property-drawer (org-text)
  "Extract a :PROPERTIES: block from ORG-TEXT, or return nil.
Returns the full drawer text including :PROPERTIES: and :END:
lines with their newlines."
  (when (string-match
         "\\(:PROPERTIES:\n\\(?::[^\n]+\n\\)*:END:\n\\)"
         org-text)
    (match-string 1 org-text)))

(defun gdocs-sync--graft-drawer-into-heading (heading-text drawer)
  "Insert DRAWER after the heading line(s) in HEADING-TEXT.
Handles planning lines (SCHEDULED, DEADLINE) that follow the
heading line."
  (let ((lines (split-string heading-text "\n" t)))
    (if (null (cdr lines))
        ;; Single line heading
        (concat (car lines) "\n" drawer)
      ;; Multi-line: heading + planning lines
      ;; Planning lines start with SCHEDULED: or DEADLINE:
      (let ((heading-part nil)
            (rest lines)
            (done nil))
        (while (and rest (not done))
          (let ((line (car rest)))
            (if (or (null heading-part)  ; first line is always heading
                    (string-match-p
                     "^\\(SCHEDULED\\|DEADLINE\\|CLOSED\\):" line))
                (progn
                  (push line heading-part)
                  (setq rest (cdr rest)))
              (setq done t))))
        (concat (mapconcat #'identity (nreverse heading-part) "\n")
                "\n" drawer
                (when rest
                  (concat (mapconcat #'identity rest "\n") "\n")))))))

(defun gdocs-sync--graft-markers (remote-elem local-elem)
  "Copy :gdocs-marker from LOCAL-ELEM onto REMOTE-ELEM.
Markers carry org-only metadata (TODO state, tags, timestamps, etc.)
that cannot be represented in Google Docs.  They survive round-trips
via named ranges.  Returns a new element plist.  If LOCAL-ELEM is
nil or has no marker, returns REMOTE-ELEM unchanged."
  (if (and local-elem (plist-get local-elem :gdocs-marker))
      ;; Build a fresh plist with the canonical IR keys from the remote
      ;; element and the local marker.  Only the keys that the diff and
      ;; conversion engines inspect are copied; any future keys must be
      ;; added here to survive the graft.
      (append (list :type (plist-get remote-elem :type)
                    :style (plist-get remote-elem :style)
                    :contents (plist-get remote-elem :contents)
                    :id (plist-get remote-elem :id)
                    :gdocs-marker (plist-get local-elem :gdocs-marker))
              (when (plist-get remote-elem :list)
                (list :list (plist-get remote-elem :list))))
    remote-elem))

;;;; Comment-aware push

(defun gdocs-sync--push-with-comment-protection (local-ir local-filtered
                                                          remote-filtered
                                                          comments start-index
                                                          buf link-ctx)
  "Push with per-element confirmation for destructive commented changes.
LOCAL-IR is the full local IR.  LOCAL-FILTERED and
REMOTE-FILTERED are title-filtered IRs for diffing.  COMMENTS is
the list of Drive comments.  START-INDEX is the body start UTF-16
index.  BUF is the originating buffer.  LINK-CTX is the link
context."
  (let* ((doc-id gdocs-sync--document-id)
         (acct gdocs-sync--account)
         (diff-ops (gdocs-diff-compute-operations
                    remote-filtered local-filtered))
         (result (gdocs-sync--filter-commented-ops
                  diff-ops remote-filtered local-filtered comments))
         (filtered-ops (plist-get result :ops))
         (declined-ops (plist-get result :declined))
         (requests (gdocs-diff-generate-from-ops
                    remote-filtered local-filtered
                    filtered-ops start-index)))
    (if (null requests)
        (gdocs-sync--push-no-changes buf)
      (gdocs-api-batch-update
       doc-id requests
       (gdocs-sync--make-comment-aware-push-callback
        local-ir remote-filtered declined-ops diff-ops buf link-ctx)
       acct
       (gdocs-sync--make-push-error-callback buf)))))

(defun gdocs-sync--make-comment-aware-push-callback (local-ir remote-filtered
                                                              declined-ops
                                                              diff-ops
                                                              buf link-ctx)
  "Return a push callback that accounts for declined deletions.
LOCAL-IR is the IR that was pushed.  REMOTE-FILTERED is the
pre-push remote IR.  DECLINED-OPS are user-declined diff ops.
DIFF-OPS is the full operation list.  BUF and LINK-CTX are for
the callback context."
  (lambda (response)
    (if (not (buffer-live-p buf))
        (message "gdocs: push completed but buffer was killed")
      (condition-case err
          (with-current-buffer buf
            (let ((gdocs-convert--link-context link-ctx))
              (if (null declined-ops)
                  (gdocs-sync--handle-push-success local-ir response)
                (let ((shadow (gdocs-sync--reconstruct-shadow
                               local-ir remote-filtered
                               declined-ops diff-ops)))
                  (gdocs-sync--handle-push-success shadow response)))))
        ((error quit)
         (if (buffer-live-p buf)
             (with-current-buffer buf
               (gdocs-sync--handle-push-error err))
           (message "Push failed: %s" (error-message-string err))))))))

(defun gdocs-sync--filter-commented-ops (diff-ops remote-ir local-ir comments)
  "Filter destructive DIFF-OPS that have at-risk comments.
Prompts the user for each at-risk destructive operation with
options to skip, delete, open the comment, or resolve and delete.
Uppercase variants (S/D/R) apply the choice to all remaining
conflicts.  Returns a plist with :ops (the filtered operation
list) and :declined (list of declined operations)."
  (let ((comment-map (gdocs-sync--map-comments-to-elements
                      comments remote-ir))
        (filtered nil)
        (declined nil)
        (default-action nil))
    (dolist (op diff-ops)
      (if (and (gdocs-sync--destructive-op-p op remote-ir local-ir)
               (gdocs-sync--op-has-at-risk-comments-p op comment-map))
          (let ((action (or default-action
                            (gdocs-sync--confirm-deletion
                             op remote-ir comment-map))))
            ;; Handle "all" variants
            (when (memq action '(skip-all delete-all resolve-all))
              (setq default-action
                    (pcase action
                      ('skip-all 'skip)
                      ('delete-all 'delete)
                      ('resolve-all 'resolve))
                    action default-action))
            (pcase action
              ('delete (push op filtered))
              ('skip (push op declined))
              ('resolve
               (gdocs-sync--resolve-element-comments
                op comment-map)
               (push op filtered))))
        (push op filtered)))
    (list :ops (nreverse filtered)
          :declined (nreverse declined))))

(defun gdocs-sync--resolve-element-comments (op comment-map)
  "Resolve all comments on the element targeted by OP.
COMMENT-MAP maps element indices to comment lists."
  (let* ((oi (plist-get op :old-index))
         (elem-comments (cdr (assq oi comment-map))))
    (dolist (c elem-comments)
      (let ((cid (alist-get 'id c)))
        (when cid
          (gdocs-api-resolve-comment
           gdocs-sync--document-id cid
           (lambda (_) nil)
           gdocs-sync--account))))))

(defun gdocs-sync--destructive-op-p (op remote-ir local-ir)
  "Return non-nil if OP is a destructive diff operation.
Destructive means element deletion or cross-type modification."
  (let ((op-type (plist-get op :op)))
    (or (eq op-type 'delete)
        (and (eq op-type 'modify)
             (let ((old-elem (nth (plist-get op :old-index) remote-ir))
                   (new-elem (nth (plist-get op :new-index) local-ir)))
               (not (eq (plist-get old-elem :type)
                        (plist-get new-elem :type))))))))

(defun gdocs-sync--map-comments-to-elements (comments remote-ir)
  "Map comments to the remote IR elements they are anchored to.
Returns an alist of (ELEMENT-INDEX . COMMENT-LIST).  Comments
without quoted text (document-level) are skipped."
  (let ((map nil))
    (dolist (comment comments)
      (unless (eq (alist-get 'deleted comment) t)
        (let* ((quoted (alist-get 'quotedFileContent comment))
               (quoted-text (and quoted (alist-get 'value quoted))))
          (when (and quoted-text (not (string-empty-p quoted-text)))
            (let ((i 0))
              (dolist (elem remote-ir)
                (let ((plain (gdocs-sync--element-plain-text elem)))
                  (when (and (not (string-empty-p plain))
                             (string-search quoted-text plain))
                    (let ((existing (assq i map)))
                      (if existing
                          (push comment (cdr existing))
                        (push (cons i (list comment)) map)))))
                (setq i (1+ i))))))))
    map))

(defun gdocs-sync--element-plain-text (element)
  "Extract plain text from ELEMENT for comment matching."
  (pcase (plist-get element :type)
    ('paragraph
     (gdocs-convert--runs-to-plain-text (plist-get element :contents)))
    ('table
     (mapconcat
      (lambda (row)
        (mapconcat #'gdocs-convert--runs-to-plain-text row " "))
      (plist-get element :rows) " "))
    ('footnote
     (gdocs-convert--runs-to-plain-text (plist-get element :contents)))
    (_ "")))

(defun gdocs-sync--op-has-at-risk-comments-p (op comment-map)
  "Return non-nil if OP targets an element with comments in COMMENT-MAP."
  (assq (plist-get op :old-index) comment-map))

(defun gdocs-sync--confirm-deletion (op remote-ir comment-map)
  "Prompt the user about a destructive operation on a commented element.
OP is the diff operation.  REMOTE-IR is the remote element list.
COMMENT-MAP maps element indices to comment lists.

Returns a symbol: `delete' to proceed (destroying the comment),
`skip' to skip this operation (preserving the comment),
`resolve' to resolve the comment and then proceed, or
`open' to open the comment URL first (re-prompts afterward)."
  (let* ((oi (plist-get op :old-index))
         (elem (nth oi remote-ir))
         (elem-type (symbol-name (plist-get elem :type)))
         (elem-text (truncate-string-to-width
                     (gdocs-sync--element-plain-text elem) 50 nil nil "..."))
         (comments (cdr (assq oi comment-map)))
         (comment-lines
          (mapconcat
           (lambda (c)
             (let ((author (alist-get 'displayName
                                      (alist-get 'author c)))
                   (content (alist-get 'content c)))
               (format "  by %s: \"%s\""
                       (or author "Unknown") (or content ""))))
           comments "\n"))
         (prompt (format
                  "Deleting %s \"%s\" would lose comment%s:\n%s\n[s]kip, [d]elete, [o]pen, [r]esolve & delete; uppercase=all? "
                  elem-type elem-text
                  (if (> (length comments) 1) "s" "")
                  comment-lines)))
    (gdocs-sync--comment-action-prompt prompt comments)))

(defun gdocs-sync--comment-action-prompt (prompt comments)
  "Display PROMPT and read a single-key action for COMMENTS.
Returns `delete', `skip', `resolve', `delete-all', `skip-all',
or `resolve-all'.  The `open' action opens the comment in a
browser and re-prompts."
  (let ((action nil))
    (while (null action)
      (let ((key (read-char-choice prompt
                                   '(?s ?d ?o ?r ?S ?D ?R))))
        (pcase key
          (?s (setq action 'skip))
          (?d (setq action 'delete))
          (?r (setq action 'resolve))
          (?S (setq action 'skip-all))
          (?D (setq action 'delete-all))
          (?R (setq action 'resolve-all))
          (?o (gdocs-sync--open-comment-in-browser (car comments))
              (message "Opened comment in browser.  Choose an action:")))))
    action))

(defun gdocs-sync--open-comment-in-browser (comment)
  "Open COMMENT in the browser using the Google Docs comment URL."
  (let* ((comment-id (alist-get 'id comment))
         (doc-id gdocs-sync--document-id)
         (url (format "https://docs.google.com/document/d/%s/edit?disco=%s"
                      doc-id comment-id)))
    (browse-url url)))

(defun gdocs-sync--reconstruct-shadow (local-ir remote-ir declined-ops
                                                diff-ops)
  "Build a shadow IR that includes declined deletion elements.
LOCAL-IR is the IR that was pushed.  REMOTE-IR is the pre-push
remote IR.  DECLINED-OPS are the user-declined operations.
DIFF-OPS is the full operation list used to determine insertion
positions."
  (if (null declined-ops)
      local-ir
    (let ((insertions nil))
      (dolist (dop declined-ops)
        (let* ((oi (plist-get dop :old-index))
               (remote-elem (nth oi remote-ir))
               (preceding-local-idx
                (gdocs-sync--preceding-local-index oi diff-ops)))
          (push (cons (if preceding-local-idx
                          (1+ preceding-local-idx)
                        0)
                      remote-elem)
                insertions)))
      ;; Insert from highest position first to preserve indices
      (setq insertions
            (sort insertions (lambda (a b) (> (car a) (car b)))))
      (let ((shadow (copy-sequence local-ir)))
        (dolist (ins insertions)
          (let ((pos (min (car ins) (length shadow)))
                (elem (cdr ins)))
            (setq shadow (append (cl-subseq shadow 0 pos)
                                 (list elem)
                                 (cl-subseq shadow pos)))))
        shadow))))

(defun gdocs-sync--preceding-local-index (old-idx diff-ops)
  "Find the local index of the nearest preceding kept/modified op.
OLD-IDX is the remote element index.  DIFF-OPS is the full
operation list.  Returns the :new-index of the nearest preceding
keep or modify operation, or nil if none precedes OLD-IDX."
  (let ((result nil))
    (dolist (op diff-ops)
      (let ((op-type (plist-get op :op))
            (oi (plist-get op :old-index)))
        (when (and oi (< oi old-idx)
                   (memq op-type '(keep modify)))
          (setq result (plist-get op :new-index)))))
    result))

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
    (setq gdocs-sync--document-id doc-id)
    (setq gdocs-sync--account acct)
    (gdocs-sync--write-properties doc-id acct)
    (gdocs--ensure-org-tag)
    (let ((before-save-hook nil)
          (after-save-hook nil))
      (save-buffer))
    (gdocs-sync-pull)))

(defun gdocs-sync-unlink ()
  "Remove Google Docs link from the current buffer."
  (interactive)
  (gdocs-sync--remove-properties)
  (gdocs--remove-org-tag)
  (gdocs-sync--clear-buffer-state)
  (let ((before-save-hook nil)
        (after-save-hook nil))
    (save-buffer))
  (message "Unlinked from Google Docs."))

(defun gdocs-sync--write-properties (doc-id account)
  "Write gdocs metadata to the file-level property drawer.
DOC-ID and ACCOUNT are required.  The current buffer-local
`gdocs-sync--revision-id' and `gdocs-sync--last-sync-time' are
also persisted when non-nil."
  (save-excursion
    (goto-char (point-min))
    (org-entry-put nil "GDOCS_DOCUMENT_ID" doc-id)
    (org-entry-put nil "GDOCS_ACCOUNT" account)
    (when gdocs-sync--revision-id
      (org-entry-put nil "GDOCS_REVISION_ID" gdocs-sync--revision-id))
    (when gdocs-sync--last-sync-time
      (org-entry-put nil "GDOCS_LAST_SYNC" gdocs-sync--last-sync-time))))

(defun gdocs-sync--reset-element-cache ()
  "Fully reset the org-element cache for the current buffer.
Unregisters the persistent cache first so that stale position
data is not reloaded on the next cache reset or file open."
  (when (derived-mode-p 'org-mode)
    (when (fboundp 'org-persist-unregister)
      (ignore-errors
        (org-persist-unregister 'org-element--cache (current-buffer)))
      (ignore-errors
        (org-persist-unregister 'org-element--headline-cache (current-buffer))))
    (org-element-cache-reset)))

(defun gdocs-sync--persist-properties ()
  "Write all current buffer-local sync state to the property drawer.
Resets the org-element cache afterward because `org-entry-put'
changes buffer positions, which invalidates cached element
boundaries and causes freezes in subsequent cache consumers
\(e.g. `org-encrypt-entries' on `before-save-hook')."
  (when gdocs-sync--document-id
    (gdocs-sync--write-properties gdocs-sync--document-id
                                  (or gdocs-sync--account ""))
    (gdocs-sync--reset-element-cache)))

(defun gdocs-sync--remove-properties ()
  "Remove all gdocs properties from the file-level property drawer."
  (save-excursion
    (goto-char (point-min))
    (org-entry-delete nil "GDOCS_DOCUMENT_ID")
    (org-entry-delete nil "GDOCS_ACCOUNT")
    (org-entry-delete nil "GDOCS_REVISION_ID")
    (org-entry-delete nil "GDOCS_LAST_SYNC"))
  (gdocs-sync--reset-element-cache))

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

(defconst gdocs-sync--folder-url-regexp
  "https://drive\\.google\\.com/drive/\\(?:u/[0-9]+/\\)?folders/\\([^/?]+\\)"
  "Regexp matching a Google Drive folder URL, capturing the folder ID.")

(defun gdocs-sync--parse-folder-id (id-or-url)
  "Extract folder ID from ID-OR-URL.
If ID-OR-URL is a Google Drive folder URL, extract the folder ID.
Otherwise return ID-OR-URL as-is."
  (if (string-match gdocs-sync--folder-url-regexp id-or-url)
      (match-string 1 id-or-url)
    id-or-url))

;;;; Status management

(defun gdocs-sync--set-status (status)
  "Set the sync status to STATUS and update the modeline."
  (setq gdocs-sync--status status)
  (when (fboundp 'gdocs--update-modeline)
    (gdocs--update-modeline)))

;;;; State initialization from property drawer

(defun gdocs-sync--init-from-properties ()
  "Initialize buffer-local sync state from the file-level property drawer."
  (save-excursion
    (goto-char (point-min))
    (when-let* ((doc-id (org-entry-get nil "GDOCS_DOCUMENT_ID")))
      (setq gdocs-sync--document-id doc-id))
    (when-let* ((acct (org-entry-get nil "GDOCS_ACCOUNT")))
      (setq gdocs-sync--account acct))
    (when-let* ((rev (org-entry-get nil "GDOCS_REVISION_ID")))
      (setq gdocs-sync--revision-id rev))
    (when-let* ((ts (org-entry-get nil "GDOCS_LAST_SYNC")))
      (setq gdocs-sync--last-sync-time ts))))

;;;; Title filtering

(defun gdocs-sync--body-start-index (ir)
  "Compute the UTF-16 index where non-title body content starts.
Title paragraphs from the document body occupy space but are
filtered from the IR before diffing.  Synthetic title elements
created from document metadata (`:source \\='metadata') do NOT
occupy body space and are excluded from the offset."
  (let ((offset 0))
    (dolist (element ir)
      (when (and (eq (plist-get element :style) 'title)
                 (not (eq (plist-get element :source) 'metadata)))
        (setq offset (+ offset (gdocs-diff--element-utf16-length element)))))
    ;; 1 = Google Docs body start index (index 0 is the document root)
    (+ 1 offset)))

(defun gdocs-sync--filter-empty-paragraphs (ir)
  "Remove empty paragraphs from IR.
Google Docs inserts structural empty paragraphs (e.g. between
content elements) that cannot be deleted via the API.  Filtering
them from the remote IR prevents the diff engine from generating
doomed delete requests."
  (cl-remove-if (lambda (element)
                  (and (eq (plist-get element :type) 'paragraph)
                       (not (plist-get element :list))
                       (let ((contents (plist-get element :contents)))
                         (or (null contents)
                             (and (= (length contents) 1)
                                  (string-empty-p
                                   (string-trim
                                    (or (plist-get (car contents) :text)
                                        ""))))))))
                ir))

(defun gdocs-sync--filter-title (ir)
  "Return IR with title elements removed.
The document title comes from metadata, not the body.  Including
it in the IR causes index misalignment during diff and push."
  (cl-remove-if (lambda (element)
                  (eq (plist-get element :style) 'title))
                ir))

(defun gdocs-sync--extract-title (ir)
  "Return the title text from IR, or nil if no title element exists."
  (cl-loop for element in ir
           when (eq (plist-get element :style) 'title)
           return (gdocs-convert--runs-to-plain-text
                   (plist-get element :contents))))

(defun gdocs-sync--maybe-rename-document (local-ir remote-title
                                                   doc-id account)
  "Rename the Google Doc if the local title differs from REMOTE-TITLE.
LOCAL-IR is the current org buffer's IR.  DOC-ID and ACCOUNT
identify the document."
  (let ((local-title (gdocs-sync--extract-title local-ir)))
    (when (and local-title
               (not (equal local-title remote-title)))
      (gdocs-api-rename-file
       doc-id local-title
       (lambda (_response)
         (message "Renamed document to: %s" local-title))
       account))))

(provide 'gdocs-sync)
;;; gdocs-sync.el ends here
