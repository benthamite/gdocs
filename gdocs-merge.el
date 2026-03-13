;;; gdocs-merge.el --- Conflict resolution UI for gdocs -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Pablo Stafforini

;; Author: gdocs contributors

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Side-by-side conflict resolution interface for resolving merge
;; conflicts between local and remote versions of a Google Doc.
;; Displays hunks with color-coded overlays and lets the user accept
;; local, remote, or manually edited text for each hunk.

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 's)
(require 'diff)

;;;; Faces

(defface gdocs-merge-local-face
  '((t :background "#e6ffe6"))
  "Face for local changes in merge view.")

(defface gdocs-merge-remote-face
  '((t :background "#ffe6e6"))
  "Face for remote changes in merge view.")

(defface gdocs-merge-resolved-face
  '((t :background "#e6e6ff"))
  "Face for resolved hunks in merge view.")

;;;; Buffer-local state

(defvar-local gdocs-merge--hunks nil
  "List of hunk plists for the current merge session.
Each hunk is (:local-text STR :remote-text STR :choice SYMBOL
:edited-text STR-OR-NIL :overlay OBJ).")

(defvar-local gdocs-merge--callback nil
  "Callback function to invoke with the merged string.")

(defvar-local gdocs-merge--current-hunk-index 0
  "Index of the currently selected hunk.")

;;;; Keymap

(defvar gdocs-merge-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a") #'gdocs-merge-accept-local)
    (define-key map (kbd "b") #'gdocs-merge-accept-remote)
    (define-key map (kbd "e") #'gdocs-merge-edit-hunk)
    (define-key map (kbd "n") #'gdocs-merge-next-hunk)
    (define-key map (kbd "p") #'gdocs-merge-previous-hunk)
    (define-key map (kbd "A") #'gdocs-merge-accept-all-local)
    (define-key map (kbd "B") #'gdocs-merge-accept-all-remote)
    (define-key map (kbd "C-c C-c") #'gdocs-merge-finalize)
    (define-key map (kbd "C-c C-k") #'gdocs-merge-abort)
    (define-key map (kbd "q") #'gdocs-merge-abort)
    map))

;;;; Major mode

(define-derived-mode gdocs-merge-mode special-mode "GDocs-Merge"
  "Major mode for resolving gdocs merge conflicts.
\\{gdocs-merge-mode-map}")

;;;; Entry point

(defun gdocs-merge-start (local-org-string remote-org-string callback)
  "Show a merge view for LOCAL-ORG-STRING vs REMOTE-ORG-STRING.
CALLBACK is called with the final merged org string when the user
finalizes the merge."
  (let* ((hunks (gdocs-merge--compute-hunks local-org-string remote-org-string))
         (buf (gdocs-merge--setup-buffer hunks callback)))
    (pop-to-buffer buf)
    (message "Resolve conflicts: a=local, b=remote, e=edit, n/p=navigate, C-c C-c=finalize")))

;;;; Hunk computation

(defun gdocs-merge--compute-hunks (local-string remote-string)
  "Compute a list of hunks from LOCAL-STRING and REMOTE-STRING.
Each hunk is a plist with :local-text, :remote-text, :choice nil,
:edited-text nil, and :overlay nil."
  (let ((local-lines (s-lines local-string))
        (remote-lines (s-lines remote-string)))
    (gdocs-merge--diff-lines-to-hunks local-lines remote-lines)))

(defun gdocs-merge--diff-lines-to-hunks (local-lines remote-lines)
  "Diff LOCAL-LINES against REMOTE-LINES and return a list of hunks."
  (let* ((lcs (gdocs-merge--lcs local-lines remote-lines))
         (hunks nil)
         (li 0)
         (ri 0))
    (dolist (common-line lcs)
      (let ((local-chunk nil)
            (remote-chunk nil))
        (while (not (equal (nth li local-lines) common-line))
          (push (nth li local-lines) local-chunk)
          (cl-incf li))
        (while (not (equal (nth ri remote-lines) common-line))
          (push (nth ri remote-lines) remote-chunk)
          (cl-incf ri))
        (when (or local-chunk remote-chunk)
          (push (gdocs-merge--make-hunk
                 (s-join "\n" (nreverse local-chunk))
                 (s-join "\n" (nreverse remote-chunk)))
                hunks))
        (push (gdocs-merge--make-common-hunk common-line) hunks)
        (cl-incf li)
        (cl-incf ri)))
    ;; Collect trailing lines after the last LCS match
    (let ((local-tail nil)
          (remote-tail nil))
      (while (< li (length local-lines))
        (push (nth li local-lines) local-tail)
        (cl-incf li))
      (while (< ri (length remote-lines))
        (push (nth ri remote-lines) remote-tail)
        (cl-incf ri))
      (when (or local-tail remote-tail)
        (push (gdocs-merge--make-hunk
               (s-join "\n" (nreverse local-tail))
               (s-join "\n" (nreverse remote-tail)))
              hunks)))
    (nreverse hunks)))

(defun gdocs-merge--make-hunk (local-text remote-text)
  "Create a conflict hunk plist from LOCAL-TEXT and REMOTE-TEXT."
  (list :local-text local-text
        :remote-text remote-text
        :choice nil
        :edited-text nil
        :overlay nil))

(defun gdocs-merge--make-common-hunk (line)
  "Create a common (non-conflicting) hunk for LINE."
  (list :local-text line
        :remote-text line
        :choice 'both
        :edited-text nil
        :overlay nil))

;;;; LCS (longest common subsequence)

(defun gdocs-merge--lcs (xs ys)
  "Compute the longest common subsequence of lists XS and YS.
Elements are compared with `equal'.  Return a list."
  (let* ((m (length xs))
         (n (length ys))
         (table (make-vector (1+ m) nil)))
    (dotimes (i (1+ m))
      (aset table i (make-vector (1+ n) 0)))
    (dotimes (i m)
      (dotimes (j n)
        (if (equal (nth i xs) (nth j ys))
            (aset (aref table (1+ i)) (1+ j)
                  (1+ (aref (aref table i) j)))
          (aset (aref table (1+ i)) (1+ j)
                (max (aref (aref table i) (1+ j))
                     (aref (aref table (1+ i)) j))))))
    (gdocs-merge--lcs-backtrack table xs ys m n)))

(defun gdocs-merge--lcs-backtrack (table xs ys m n)
  "Backtrack through TABLE to extract the LCS.
XS and YS are the original lists.  M and N are their lengths."
  (let ((result nil)
        (i m)
        (j n))
    (while (and (> i 0) (> j 0))
      (cond
       ((equal (nth (1- i) xs) (nth (1- j) ys))
        (push (nth (1- i) xs) result)
        (setq i (1- i)
              j (1- j)))
       ((> (aref (aref table (1- i)) j)
           (aref (aref table i) (1- j)))
        (setq i (1- i)))
       (t
        (setq j (1- j)))))
    result))

;;;; Buffer setup

(defun gdocs-merge--setup-buffer (hunks callback)
  "Set up the merge buffer with HUNKS and CALLBACK.  Return the buffer."
  (let ((buf (get-buffer-create "*gdocs-merge*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (gdocs-merge-mode)
        (setq gdocs-merge--hunks hunks)
        (setq gdocs-merge--callback callback)
        (setq gdocs-merge--current-hunk-index 0)
        (gdocs-merge--render-hunks)
        (goto-char (point-min))))
    buf))

(defun gdocs-merge--render-hunks ()
  "Render all hunks into the merge buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (dolist (hunk gdocs-merge--hunks)
      (gdocs-merge--render-one-hunk hunk))))

(defun gdocs-merge--render-one-hunk (hunk)
  "Render a single HUNK into the buffer at point."
  (let ((choice (plist-get hunk :choice))
        (local-text (plist-get hunk :local-text))
        (remote-text (plist-get hunk :remote-text))
        (start (point)))
    (cond
     ((eq choice 'both)
      (insert local-text "\n"))
     ((eq choice 'local)
      (gdocs-merge--insert-resolved local-text hunk start))
     ((eq choice 'remote)
      (gdocs-merge--insert-resolved remote-text hunk start))
     ((eq choice 'edited)
      (gdocs-merge--insert-resolved (plist-get hunk :edited-text) hunk start))
     (t
      (gdocs-merge--insert-conflict local-text remote-text hunk start)))))

(defun gdocs-merge--insert-resolved (text hunk start)
  "Insert resolved TEXT for HUNK starting at START."
  (insert text "\n")
  (let ((ov (make-overlay start (point))))
    (overlay-put ov 'face 'gdocs-merge-resolved-face)
    (plist-put hunk :overlay ov)))

(defun gdocs-merge--insert-conflict (local-text remote-text hunk start)
  "Insert conflicting LOCAL-TEXT and REMOTE-TEXT for HUNK at START."
  (insert "<<<< LOCAL\n")
  (insert local-text "\n")
  (insert "====\n")
  (insert remote-text "\n")
  (insert ">>>> REMOTE\n")
  (let ((ov (make-overlay start (point))))
    (overlay-put ov 'face 'gdocs-merge-remote-face)
    (plist-put hunk :overlay ov)))

;;;; Navigation

(defun gdocs-merge--conflict-hunks ()
  "Return the indices of unresolved conflict hunks."
  (let ((indices nil))
    (cl-loop for i from 0
             for hunk in gdocs-merge--hunks
             unless (plist-get hunk :choice)
             do (push i indices))
    (nreverse indices)))

(defun gdocs-merge-next-hunk ()
  "Move to the next conflict hunk."
  (interactive)
  (let* ((conflicts (gdocs-merge--conflict-hunks))
         (next (--first (> it gdocs-merge--current-hunk-index) conflicts)))
    (if next
        (gdocs-merge--goto-hunk next)
      (message "No more conflict hunks"))))

(defun gdocs-merge-previous-hunk ()
  "Move to the previous conflict hunk."
  (interactive)
  (let* ((conflicts (gdocs-merge--conflict-hunks))
         (prev (-last-item (--filter (< it gdocs-merge--current-hunk-index)
                                     conflicts))))
    (if prev
        (gdocs-merge--goto-hunk prev)
      (message "No previous conflict hunks"))))

(defun gdocs-merge--goto-hunk (index)
  "Move point to hunk at INDEX."
  (setq gdocs-merge--current-hunk-index index)
  (let ((hunk (nth index gdocs-merge--hunks)))
    (when-let* ((ov (plist-get hunk :overlay)))
      (goto-char (overlay-start ov)))))

;;;; Accepting changes

(defun gdocs-merge-accept-local ()
  "Accept the local version for the current hunk."
  (interactive)
  (gdocs-merge--resolve-current-hunk 'local))

(defun gdocs-merge-accept-remote ()
  "Accept the remote version for the current hunk."
  (interactive)
  (gdocs-merge--resolve-current-hunk 'remote))

(defun gdocs-merge--resolve-current-hunk (choice)
  "Resolve the current hunk with CHOICE (`local' or `remote')."
  (let ((hunk (nth gdocs-merge--current-hunk-index gdocs-merge--hunks)))
    (unless hunk
      (user-error "No hunk at current position"))
    (plist-put hunk :choice choice)
    (gdocs-merge--render-hunks)
    (gdocs-merge-next-hunk)))

(defun gdocs-merge-edit-hunk ()
  "Edit the merged result for the current hunk manually."
  (interactive)
  (let* ((hunk (nth gdocs-merge--current-hunk-index gdocs-merge--hunks))
         (local-text (plist-get hunk :local-text))
         (buf (generate-new-buffer "*gdocs-merge-edit*")))
    (with-current-buffer buf
      (insert local-text)
      (org-mode)
      (local-set-key (kbd "C-c C-c")
                     (gdocs-merge--make-edit-finalizer hunk buf)))
    (pop-to-buffer buf)
    (message "Edit the text, then press C-c C-c to accept")))

(defun gdocs-merge--make-edit-finalizer (hunk edit-buf)
  "Return a command that finalizes editing HUNK from EDIT-BUF."
  (lambda ()
    (interactive)
    (let ((text (with-current-buffer edit-buf
                  (buffer-substring-no-properties (point-min) (point-max)))))
      (plist-put hunk :choice 'edited)
      (plist-put hunk :edited-text (s-trim-right text))
      (kill-buffer edit-buf)
      (when-let* ((merge-buf (get-buffer "*gdocs-merge*")))
        (pop-to-buffer merge-buf)
        (gdocs-merge--render-hunks)))))

(defun gdocs-merge-accept-all-local ()
  "Accept the local version for all unresolved hunks."
  (interactive)
  (gdocs-merge--resolve-all 'local))

(defun gdocs-merge-accept-all-remote ()
  "Accept the remote version for all unresolved hunks."
  (interactive)
  (gdocs-merge--resolve-all 'remote))

(defun gdocs-merge--resolve-all (choice)
  "Set CHOICE on all unresolved hunks and re-render."
  (dolist (hunk gdocs-merge--hunks)
    (unless (plist-get hunk :choice)
      (plist-put hunk :choice choice)))
  (gdocs-merge--render-hunks))

;;;; Finalization

(defun gdocs-merge-finalize ()
  "Finalize the merge and call the callback with the result."
  (interactive)
  (gdocs-merge--validate-all-resolved)
  (let ((result (gdocs-merge--build-result))
        (callback gdocs-merge--callback))
    (kill-buffer (current-buffer))
    (funcall callback result)))

(defun gdocs-merge--validate-all-resolved ()
  "Signal an error if any hunks are unresolved."
  (let ((unresolved (--count (null (plist-get it :choice))
                             gdocs-merge--hunks)))
    (when (> unresolved 0)
      (user-error "%d unresolved hunk(s) remaining" unresolved))))

(defun gdocs-merge--build-result ()
  "Concatenate all resolved hunks into the final org string."
  (let ((parts nil))
    (dolist (hunk gdocs-merge--hunks)
      (push (gdocs-merge--hunk-resolved-text hunk) parts))
    (s-join "\n" (nreverse parts))))

(defun gdocs-merge--hunk-resolved-text (hunk)
  "Return the resolved text for HUNK."
  (pcase (plist-get hunk :choice)
    ('local (plist-get hunk :local-text))
    ('remote (plist-get hunk :remote-text))
    ('edited (plist-get hunk :edited-text))
    ('both (plist-get hunk :local-text))))

(defun gdocs-merge-abort ()
  "Abort the merge without calling the callback."
  (interactive)
  (when (yes-or-no-p "Abort merge? ")
    (kill-buffer (current-buffer))
    (message "Merge aborted.")))

(provide 'gdocs-merge)
;;; gdocs-merge.el ends here
