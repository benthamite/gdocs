;;; gdocs-merge-test.el --- Tests for gdocs-merge -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Pablo Stafforini

;; Author: Pablo Stafforini

;; This file is not part of GNU Emacs.

;;; Commentary:

;; ERT tests for the merge conflict resolution module.  Covers hunk
;; computation, hunk helpers, buffer rendering, navigation, accept
;; operations, edit hunk, finalization, and result building.

;;; Code:

(require 'ert)
(require 'gdocs-merge)

;;;; Test helpers

(defun gdocs-merge-test--make-hunks (local remote)
  "Compute hunks from LOCAL and REMOTE strings for testing."
  (gdocs-merge--compute-hunks local remote))

(defmacro gdocs-merge-test--with-merge-buffer (hunks &rest body)
  "Set up a merge buffer with HUNKS and execute BODY.
Cleans up the buffer afterwards."
  (declare (indent 1) (debug t))
  `(let ((buf (gdocs-merge--setup-buffer ,hunks #'ignore)))
     (unwind-protect
         (with-current-buffer buf
           ,@body)
       (when (buffer-live-p buf)
         (kill-buffer buf)))))

;;;; Hunk creation helpers

(ert-deftest gdocs-merge-test-make-hunk ()
  "make-hunk creates a conflict plist with nil choice."
  (let ((hunk (gdocs-merge--make-hunk "local text" "remote text")))
    (should (equal (plist-get hunk :local-text) "local text"))
    (should (equal (plist-get hunk :remote-text) "remote text"))
    (should (null (plist-get hunk :choice)))
    (should (null (plist-get hunk :edited-text)))
    (should (null (plist-get hunk :overlay)))))

(ert-deftest gdocs-merge-test-make-common-hunk ()
  "make-common-hunk creates a hunk with choice set to common."
  (let ((hunk (gdocs-merge--make-common-hunk "shared line")))
    (should (equal (plist-get hunk :local-text) "shared line"))
    (should (equal (plist-get hunk :remote-text) "shared line"))
    (should (eq (plist-get hunk :choice) 'common))
    (should (null (plist-get hunk :edited-text)))
    (should (null (plist-get hunk :overlay)))))

;;;; Hunk computation

(ert-deftest gdocs-merge-test-identical-strings ()
  "Identical strings produce only common hunks."
  (let* ((text "line one\nline two\nline three")
         (hunks (gdocs-merge-test--make-hunks text text)))
    (should (> (length hunks) 0))
    (dolist (hunk hunks)
      (should (eq (plist-get hunk :choice) 'common)))))

(ert-deftest gdocs-merge-test-single-line-differs ()
  "A single differing line produces a conflict hunk between common hunks."
  (let* ((local "alpha\nbeta\ngamma")
         (remote "alpha\nBETA\ngamma")
         (hunks (gdocs-merge-test--make-hunks local remote))
         (conflict-hunks (cl-remove-if
                          (lambda (h) (plist-get h :choice))
                          hunks)))
    (should (= (length conflict-hunks) 1))
    (should (equal (plist-get (car conflict-hunks) :local-text) "beta"))
    (should (equal (plist-get (car conflict-hunks) :remote-text) "BETA"))))

(ert-deftest gdocs-merge-test-multiple-differing-regions ()
  "Multiple differing regions with common lines between them."
  (let* ((local "A\nB\nC\nD\nE")
         (remote "A\nX\nC\nY\nE")
         (hunks (gdocs-merge-test--make-hunks local remote))
         (conflict-hunks (cl-remove-if
                          (lambda (h) (plist-get h :choice))
                          hunks)))
    (should (= (length conflict-hunks) 2))
    (should (equal (plist-get (car conflict-hunks) :local-text) "B"))
    (should (equal (plist-get (car conflict-hunks) :remote-text) "X"))
    (should (equal (plist-get (cadr conflict-hunks) :local-text) "D"))
    (should (equal (plist-get (cadr conflict-hunks) :remote-text) "Y"))))

(ert-deftest gdocs-merge-test-trailing-lines-after-lcs ()
  "Trailing lines after the last LCS match produce a conflict hunk."
  (let* ((local "shared\nlocal-tail")
         (remote "shared\nremote-tail")
         (hunks (gdocs-merge-test--make-hunks local remote))
         (conflict-hunks (cl-remove-if
                          (lambda (h) (plist-get h :choice))
                          hunks)))
    (should (= (length conflict-hunks) 1))
    (should (equal (plist-get (car conflict-hunks) :local-text) "local-tail"))
    (should (equal (plist-get (car conflict-hunks) :remote-text) "remote-tail"))))

(ert-deftest gdocs-merge-test-empty-local ()
  "Empty local text produces a conflict hunk with empty local side."
  (let* ((hunks (gdocs-merge-test--make-hunks "" "some remote text")))
    ;; Should have at least one conflict hunk
    (let ((conflict-hunks (cl-remove-if
                           (lambda (h) (plist-get h :choice))
                           hunks)))
      (should (> (length conflict-hunks) 0)))))

(ert-deftest gdocs-merge-test-empty-remote ()
  "Empty remote text produces a conflict hunk with empty remote side."
  (let* ((hunks (gdocs-merge-test--make-hunks "some local text" "")))
    (let ((conflict-hunks (cl-remove-if
                           (lambda (h) (plist-get h :choice))
                           hunks)))
      (should (> (length conflict-hunks) 0)))))

(ert-deftest gdocs-merge-test-completely-different ()
  "Completely different content produces conflict hunks with no common hunks."
  (let* ((local "aaa\nbbb\nccc")
         (remote "xxx\nyyy\nzzz")
         (hunks (gdocs-merge-test--make-hunks local remote))
         (common-hunks (cl-remove-if-not
                        (lambda (h) (eq (plist-get h :choice) 'common))
                        hunks)))
    (should (= (length common-hunks) 0))
    (should (> (length hunks) 0))))

;;;; Buffer setup and rendering

(ert-deftest gdocs-merge-test-buffer-mode ()
  "Setup buffer uses gdocs-merge-mode."
  (let* ((hunks (list (gdocs-merge--make-common-hunk "hello")))
         (buf (gdocs-merge--setup-buffer hunks #'ignore)))
    (unwind-protect
        (with-current-buffer buf
          (should (eq major-mode 'gdocs-merge-mode)))
      (kill-buffer buf))))

(ert-deftest gdocs-merge-test-conflict-renders-markers ()
  "Conflict hunks render with <<<< LOCAL markers."
  (let* ((hunks (list (gdocs-merge--make-hunk "local-side" "remote-side"))))
    (gdocs-merge-test--with-merge-buffer hunks
      (let ((content (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "<<<< LOCAL" content))
        (should (string-match-p "local-side" content))
        (should (string-match-p "====" content))
        (should (string-match-p "remote-side" content))
        (should (string-match-p ">>>> REMOTE" content))))))

(ert-deftest gdocs-merge-test-common-renders-plain ()
  "Common hunks render as plain text without conflict markers."
  (let* ((hunks (list (gdocs-merge--make-common-hunk "plain line"))))
    (gdocs-merge-test--with-merge-buffer hunks
      (let ((content (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "plain line" content))
        (should-not (string-match-p "<<<< LOCAL" content))
        (should-not (string-match-p ">>>> REMOTE" content))))))

(ert-deftest gdocs-merge-test-resolved-hunk-renders-with-overlay ()
  "Resolved hunks render with the resolved face overlay."
  (let* ((hunk (gdocs-merge--make-hunk "local" "remote")))
    (plist-put hunk :choice 'local)
    (gdocs-merge-test--with-merge-buffer (list hunk)
      (let ((content (buffer-substring-no-properties (point-min) (point-max))))
        ;; Should show the local text without conflict markers
        (should (string-match-p "local" content))
        (should-not (string-match-p "<<<< LOCAL" content)))
      ;; Should have an overlay with the resolved face
      (let ((ovs (overlays-in (point-min) (point-max))))
        (should (cl-some (lambda (ov)
                           (eq (overlay-get ov 'face)
                               'gdocs-merge-resolved-face))
                         ovs))))))

;;;; Navigation

(ert-deftest gdocs-merge-test-conflict-hunks-finds-unresolved ()
  "conflict-hunks returns indices of unresolved (nil choice) hunks."
  (let* ((hunks (list (gdocs-merge--make-common-hunk "common")
                      (gdocs-merge--make-hunk "L1" "R1")
                      (gdocs-merge--make-common-hunk "common2")
                      (gdocs-merge--make-hunk "L2" "R2"))))
    (gdocs-merge-test--with-merge-buffer hunks
      (let ((conflicts (gdocs-merge--conflict-hunks)))
        (should (equal conflicts '(1 3)))))))

(ert-deftest gdocs-merge-test-next-hunk-advances ()
  "next-hunk advances to the next conflict."
  (let* ((hunks (list (gdocs-merge--make-hunk "L1" "R1")
                      (gdocs-merge--make-common-hunk "common")
                      (gdocs-merge--make-hunk "L2" "R2"))))
    (gdocs-merge-test--with-merge-buffer hunks
      (setq gdocs-merge--current-hunk-index 0)
      (gdocs-merge-next-hunk)
      (should (= gdocs-merge--current-hunk-index 2)))))

(ert-deftest gdocs-merge-test-previous-hunk-goes-back ()
  "previous-hunk goes back to the previous conflict."
  (let* ((hunks (list (gdocs-merge--make-hunk "L1" "R1")
                      (gdocs-merge--make-common-hunk "common")
                      (gdocs-merge--make-hunk "L2" "R2"))))
    (gdocs-merge-test--with-merge-buffer hunks
      (setq gdocs-merge--current-hunk-index 2)
      (gdocs-merge-previous-hunk)
      (should (= gdocs-merge--current-hunk-index 0)))))

(ert-deftest gdocs-merge-test-next-hunk-no-more ()
  "next-hunk messages when no more conflicts exist."
  (let* ((hunks (list (gdocs-merge--make-hunk "L1" "R1"))))
    (gdocs-merge-test--with-merge-buffer hunks
      (setq gdocs-merge--current-hunk-index 0)
      ;; No conflict after index 0, so next-hunk should message
      (let ((messages nil))
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (push (apply #'format fmt args) messages))))
          (gdocs-merge-next-hunk))
        (should (cl-some (lambda (m) (string-match-p "No more" m))
                         messages))))))

(ert-deftest gdocs-merge-test-previous-hunk-no-previous ()
  "previous-hunk messages when no previous conflicts exist."
  (let* ((hunks (list (gdocs-merge--make-hunk "L1" "R1"))))
    (gdocs-merge-test--with-merge-buffer hunks
      (setq gdocs-merge--current-hunk-index 0)
      (let ((messages nil))
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (push (apply #'format fmt args) messages))))
          (gdocs-merge-previous-hunk))
        (should (cl-some (lambda (m) (string-match-p "No previous" m))
                         messages))))))

;;;; Accept operations

(ert-deftest gdocs-merge-test-accept-local ()
  "Accept local sets :choice to local on the current hunk."
  (let* ((hunks (list (gdocs-merge--make-hunk "local-v" "remote-v"))))
    (gdocs-merge-test--with-merge-buffer hunks
      (setq gdocs-merge--current-hunk-index 0)
      ;; Suppress navigation messages after resolve
      (cl-letf (((symbol-function 'message) #'ignore))
        (gdocs-merge-accept-local))
      (should (eq (plist-get (car gdocs-merge--hunks) :choice) 'local)))))

(ert-deftest gdocs-merge-test-accept-remote ()
  "Accept remote sets :choice to remote on the current hunk."
  (let* ((hunks (list (gdocs-merge--make-hunk "local-v" "remote-v"))))
    (gdocs-merge-test--with-merge-buffer hunks
      (setq gdocs-merge--current-hunk-index 0)
      (cl-letf (((symbol-function 'message) #'ignore))
        (gdocs-merge-accept-remote))
      (should (eq (plist-get (car gdocs-merge--hunks) :choice) 'remote)))))

(ert-deftest gdocs-merge-test-accept-all-local ()
  "Accept all local resolves all unresolved hunks with local choice."
  (let* ((hunks (list (gdocs-merge--make-hunk "L1" "R1")
                      (gdocs-merge--make-common-hunk "common")
                      (gdocs-merge--make-hunk "L2" "R2"))))
    (gdocs-merge-test--with-merge-buffer hunks
      (gdocs-merge--resolve-all 'local)
      (should (eq (plist-get (nth 0 gdocs-merge--hunks) :choice) 'local))
      ;; Common hunk should remain common (it already had a choice)
      (should (eq (plist-get (nth 1 gdocs-merge--hunks) :choice) 'common))
      (should (eq (plist-get (nth 2 gdocs-merge--hunks) :choice) 'local)))))

(ert-deftest gdocs-merge-test-accept-all-remote ()
  "Accept all remote resolves all unresolved hunks with remote choice."
  (let* ((hunks (list (gdocs-merge--make-hunk "L1" "R1")
                      (gdocs-merge--make-common-hunk "common")
                      (gdocs-merge--make-hunk "L2" "R2"))))
    (gdocs-merge-test--with-merge-buffer hunks
      (gdocs-merge--resolve-all 'remote)
      (should (eq (plist-get (nth 0 gdocs-merge--hunks) :choice) 'remote))
      (should (eq (plist-get (nth 1 gdocs-merge--hunks) :choice) 'common))
      (should (eq (plist-get (nth 2 gdocs-merge--hunks) :choice) 'remote)))))

(ert-deftest gdocs-merge-test-resolve-all-skips-already-resolved ()
  "resolve-all does not overwrite hunks that already have a choice."
  (let* ((hunk1 (gdocs-merge--make-hunk "L1" "R1"))
         (hunk2 (gdocs-merge--make-hunk "L2" "R2")))
    ;; Pre-resolve hunk1 as local
    (plist-put hunk1 :choice 'local)
    (gdocs-merge-test--with-merge-buffer (list hunk1 hunk2)
      (gdocs-merge--resolve-all 'remote)
      ;; hunk1 should still be local since it was already resolved
      (should (eq (plist-get (nth 0 gdocs-merge--hunks) :choice) 'local))
      (should (eq (plist-get (nth 1 gdocs-merge--hunks) :choice) 'remote)))))

;;;; Edit hunk

(ert-deftest gdocs-merge-test-edit-hunk-creates-buffer ()
  "edit-hunk creates an edit buffer pre-populated with local text."
  (let* ((hunks (list (gdocs-merge--make-hunk "local content" "remote content"))))
    (gdocs-merge-test--with-merge-buffer hunks
      (setq gdocs-merge--current-hunk-index 0)
      (let ((edit-buf nil))
        (cl-letf (((symbol-function 'pop-to-buffer)
                   (lambda (buf) (setq edit-buf buf)))
                  ((symbol-function 'message) #'ignore))
          (gdocs-merge-edit-hunk))
        (unwind-protect
            (when edit-buf
              (with-current-buffer edit-buf
                (should (string= (buffer-substring-no-properties
                                  (point-min) (point-max))
                                 "local content"))))
          (when (and edit-buf (buffer-live-p edit-buf))
            (kill-buffer edit-buf)))))))

;;;; Hunk resolved text

(ert-deftest gdocs-merge-test-hunk-resolved-text-local ()
  "hunk-resolved-text returns local text for local choice."
  (let ((hunk (gdocs-merge--make-hunk "my-local" "my-remote")))
    (plist-put hunk :choice 'local)
    (should (equal (gdocs-merge--hunk-resolved-text hunk) "my-local"))))

(ert-deftest gdocs-merge-test-hunk-resolved-text-remote ()
  "hunk-resolved-text returns remote text for remote choice."
  (let ((hunk (gdocs-merge--make-hunk "my-local" "my-remote")))
    (plist-put hunk :choice 'remote)
    (should (equal (gdocs-merge--hunk-resolved-text hunk) "my-remote"))))

(ert-deftest gdocs-merge-test-hunk-resolved-text-edited ()
  "hunk-resolved-text returns edited text for edited choice."
  (let ((hunk (gdocs-merge--make-hunk "my-local" "my-remote")))
    (plist-put hunk :choice 'edited)
    (plist-put hunk :edited-text "custom text")
    (should (equal (gdocs-merge--hunk-resolved-text hunk) "custom text"))))

(ert-deftest gdocs-merge-test-hunk-resolved-text-common ()
  "hunk-resolved-text returns local text for common choice."
  (let ((hunk (gdocs-merge--make-common-hunk "shared")))
    (should (equal (gdocs-merge--hunk-resolved-text hunk) "shared"))))

;;;; Finalization

(ert-deftest gdocs-merge-test-build-result ()
  "build-result concatenates resolved hunks with newline separators."
  (let* ((h1 (gdocs-merge--make-common-hunk "line one"))
         (h2 (gdocs-merge--make-hunk "local two" "remote two"))
         (h3 (gdocs-merge--make-common-hunk "line three")))
    (plist-put h2 :choice 'remote)
    (gdocs-merge-test--with-merge-buffer (list h1 h2 h3)
      (let ((result (gdocs-merge--build-result)))
        (should (equal result "line one\nremote two\nline three"))))))

(ert-deftest gdocs-merge-test-build-result-with-edited ()
  "build-result includes edited text when a hunk was edited."
  (let* ((h1 (gdocs-merge--make-common-hunk "first"))
         (h2 (gdocs-merge--make-hunk "L" "R")))
    (plist-put h2 :choice 'edited)
    (plist-put h2 :edited-text "manually edited")
    (gdocs-merge-test--with-merge-buffer (list h1 h2)
      (let ((result (gdocs-merge--build-result)))
        (should (equal result "first\nmanually edited"))))))

(ert-deftest gdocs-merge-test-validate-raises-on-unresolved ()
  "validate-all-resolved signals user-error when hunks remain unresolved."
  (let* ((hunks (list (gdocs-merge--make-hunk "L" "R")
                      (gdocs-merge--make-common-hunk "ok"))))
    (gdocs-merge-test--with-merge-buffer hunks
      (should-error (gdocs-merge--validate-all-resolved)
                    :type 'user-error))))

(ert-deftest gdocs-merge-test-validate-passes-when-all-resolved ()
  "validate-all-resolved succeeds when every hunk has a choice."
  (let* ((h1 (gdocs-merge--make-common-hunk "common"))
         (h2 (gdocs-merge--make-hunk "L" "R")))
    (plist-put h2 :choice 'local)
    (gdocs-merge-test--with-merge-buffer (list h1 h2)
      ;; Should not signal an error
      (gdocs-merge--validate-all-resolved))))

(ert-deftest gdocs-merge-test-finalize-calls-callback ()
  "finalize calls the callback with the merged result."
  (let* ((h1 (gdocs-merge--make-common-hunk "alpha"))
         (h2 (gdocs-merge--make-hunk "local-beta" "remote-beta"))
         (callback-result nil))
    (plist-put h2 :choice 'local)
    (let ((buf (gdocs-merge--setup-buffer
                (list h1 h2)
                (lambda (result)
                  (setq callback-result result)))))
      (with-current-buffer buf
        (gdocs-merge-finalize))
      ;; Buffer should have been killed
      (should-not (buffer-live-p buf))
      (should (equal callback-result "alpha\nlocal-beta")))))

(ert-deftest gdocs-merge-test-finalize-errors-if-unresolved ()
  "finalize raises when unresolved hunks remain."
  (let* ((hunks (list (gdocs-merge--make-hunk "L" "R")))
         (buf (gdocs-merge--setup-buffer hunks #'ignore)))
    (unwind-protect
        (with-current-buffer buf
          (should-error (gdocs-merge-finalize) :type 'user-error))
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

;;;; Integration: end-to-end hunk computation and result building

(ert-deftest gdocs-merge-test-round-trip-identical ()
  "Identical strings round-trip through compute + build to the same text."
  (let* ((text "line one\nline two\nline three")
         (hunks (gdocs-merge--compute-hunks text text)))
    (gdocs-merge-test--with-merge-buffer hunks
      (let ((result (gdocs-merge--build-result)))
        (should (equal result text))))))

(ert-deftest gdocs-merge-test-round-trip-accept-all-local ()
  "Accepting all local hunks reproduces the local string."
  (let* ((local "alpha\nbeta\ngamma")
         (remote "alpha\nBETA\nGAMMA")
         (hunks (gdocs-merge--compute-hunks local remote)))
    (gdocs-merge-test--with-merge-buffer hunks
      (gdocs-merge--resolve-all 'local)
      (let ((result (gdocs-merge--build-result)))
        (should (equal result local))))))

(ert-deftest gdocs-merge-test-round-trip-accept-all-remote ()
  "Accepting all remote hunks reproduces the remote string."
  (let* ((local "alpha\nbeta\ngamma")
         (remote "alpha\nBETA\nGAMMA")
         (hunks (gdocs-merge--compute-hunks local remote)))
    (gdocs-merge-test--with-merge-buffer hunks
      (gdocs-merge--resolve-all 'remote)
      (let ((result (gdocs-merge--build-result)))
        (should (equal result remote))))))

(provide 'gdocs-merge-test)
;;; gdocs-merge-test.el ends here
