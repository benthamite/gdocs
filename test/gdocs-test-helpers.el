;;; gdocs-test-helpers.el --- Test infrastructure for gdocs -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Pablo Stafforini

;; Author: Pablo Stafforini

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Test helpers and mocking utilities for the gdocs test suite.
;; Provides macros for temporary token directories, token file
;; creation, mock API responses, and sample data.

;;; Code:

(require 'gdocs-auth)
(require 'cl-lib)

;;;; Temporary directory management

(defmacro gdocs-test-with-temp-dir (&rest body)
  "Execute BODY with `gdocs-token-directory' set to a temp directory.
The temporary directory is created before BODY and deleted after,
along with all its contents."
  (declare (indent 0) (debug t))
  `(let* ((temp-dir (make-temp-file "gdocs-test-" t))
          (gdocs-token-directory (file-name-as-directory temp-dir)))
     (unwind-protect
         (progn ,@body)
       (delete-directory temp-dir t))))

;;;; Token file helpers

(defun gdocs-test-write-token-file (account &optional overrides)
  "Write a mock token file for ACCOUNT to `gdocs-token-directory'.
OVERRIDES is an optional alist of fields to override in the
default token data.  Return the path to the written file."
  (let ((data (gdocs-test--merge-alist
               (gdocs-test--default-token-data)
               overrides)))
    (gdocs-auth--write-token-file account data)
    (gdocs-auth--token-file-path account)))

(defun gdocs-test--default-token-data ()
  "Return a default valid token data alist for testing."
  `((access_token . "test-access-token-12345")
    (refresh_token . "test-refresh-token-67890")
    (expires_at . ,(+ (float-time) 3600))
    (client_id . "test-client-id")
    (client_secret . "test-client-secret")))

(defun gdocs-test--merge-alist (base overrides)
  "Merge OVERRIDES into BASE alist, with OVERRIDES taking precedence.
Return a new alist."
  (let ((result (copy-alist base)))
    (dolist (pair overrides result)
      (setf (alist-get (car pair) result) (cdr pair)))))

;;;; Mock API infrastructure

(defvar gdocs-test--mock-routes nil
  "Alist of (URL-REGEXP . HANDLER) for mock API dispatch.
Each HANDLER is a function called with (METHOD URL BODY) that
returns a response alist or signals an error.")

(defmacro gdocs-test-with-mock-api (routes &rest body)
  "Execute BODY with `plz' mocked via URL-based routing.
ROUTES is an alist of (URL-REGEXP . HANDLER) pairs.  When the
mock receives a request, it matches the URL against each regexp
and calls the first matching handler with (METHOD URL BODY).
The handler should return a parsed JSON alist which is passed to
the request's :then callback."
  (declare (indent 1) (debug t))
  `(let ((gdocs-test--mock-routes ,routes))
     (cl-letf (((symbol-function 'plz)
                (lambda (_method _url &rest args)
                  (gdocs-test--dispatch-mock-plz _method _url args))))
       ,@body)))

(defun gdocs-test--dispatch-mock-plz (method url args)
  "Dispatch a mocked plz call for METHOD and URL using ARGS.
Find the matching route handler and call the :then callback with
the handler's return value."
  (let* ((then-fn (plist-get args :then))
         (handler (gdocs-test--find-route-handler url)))
    (unless handler
      (error "No mock route matches URL: %s" url))
    (when then-fn
      (funcall then-fn
               (funcall handler method url (plist-get args :body))))))

(defun gdocs-test--find-route-handler (url)
  "Find the first handler in `gdocs-test--mock-routes' matching URL.
Return the handler function, or nil if no route matches."
  (cl-loop for (pattern . handler) in gdocs-test--mock-routes
           when (string-match-p pattern url)
           return handler))

;;;; Mock plz for direct plz overriding

(defmacro gdocs-test-mock-plz (response-fn &rest body)
  "Execute BODY with `plz' mocked to use RESPONSE-FN.
RESPONSE-FN is called with (METHOD URL &rest ARGS) and should
behave like plz, calling the :then or :else callbacks as
appropriate."
  (declare (indent 1) (debug t))
  `(cl-letf (((symbol-function 'plz) ,response-fn))
     ,@body))

;;;; Sample data

(defun gdocs-test-sample-document-json ()
  "Return a sample Google Docs document JSON alist for testing."
  '((documentId . "1BxiMVs0XRA5nFMdKvBdBZjgmUUqptlbs74OgVE2upms")
    (title . "Test Document")
    (body
     . ((content
         . [((sectionBreak
              . ((sectionStyle . ()))))
             ((paragraph
               . ((elements
                   . [((textRun
                        . ((content . "Hello World\n")
                           (textStyle . ()))))])
                  (paragraphStyle
                   . ((namedStyleType . "HEADING_1"))))))])))))

(defun gdocs-test-sample-metadata-json ()
  "Return sample Google Drive file metadata for testing."
  '((id . "1BxiMVs0XRA5nFMdKvBdBZjgmUUqptlbs74OgVE2upms")
    (name . "Test Document")
    (modifiedTime . "2026-01-15T10:30:00.000Z")
    (headRevisionId . "ALm37BVTxyz123")))

(provide 'gdocs-test-helpers)
;;; gdocs-test-helpers.el ends here
