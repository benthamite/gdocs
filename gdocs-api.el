;;; gdocs-api.el --- Google Docs and Drive API client -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Pablo Stafforini

;; Author: Pablo Stafforini

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Async Google Docs and Google Drive API client built on plz.el.
;; All functions are asynchronous, taking a CALLBACK parameter that
;; receives the parsed JSON response.  Error handling includes retry
;; with exponential backoff for transient errors (429, 500, 503).
;; Authentication failures (401, 403) produce clear error messages
;; directing the user to re-authenticate.

;;; Code:

(require 'gdocs-auth)
(require 'json)
(require 'plz)
(require 'cl-lib)
(require 'url-util)

;;;; Constants

(defconst gdocs-api--docs-base-url
  "https://docs.googleapis.com/v1/documents"
  "Base URL for the Google Docs API.")

(defconst gdocs-api--drive-base-url
  "https://www.googleapis.com/drive/v3/files"
  "Base URL for the Google Drive API.")

(defconst gdocs-api--drive-upload-url
  "https://www.googleapis.com/upload/drive/v3/files?uploadType=multipart"
  "URL for multipart file uploads to Google Drive.")

(defconst gdocs-api--max-retries 3
  "Maximum number of retry attempts for transient errors.")

(defconst gdocs-api--max-backoff 30
  "Maximum backoff delay in seconds.")

(defconst gdocs-api--retryable-statuses '(429 500 503)
  "HTTP status codes that trigger a retry with backoff.")

;;;; Public API — Google Docs

(defun gdocs-api-get-document (document-id callback &optional account on-error)
  "Fetch a Google Docs document by DOCUMENT-ID.
CALLBACK is called with the parsed JSON document structure.
ACCOUNT is an optional account name; if nil, the default account
is selected.  ON-ERROR, if non-nil, is called with the error
condition instead of signaling."
  (gdocs-api--request 'get
                      (concat gdocs-api--docs-base-url "/" document-id)
                      callback
                      :account account
                      :on-error on-error))

(defun gdocs-api-batch-update (document-id requests callback
                                           &optional account on-error)
  "Send a batchUpdate to DOCUMENT-ID with REQUESTS.
REQUESTS is a list of request alists.  CALLBACK is called with
the parsed JSON response.  ACCOUNT is an optional account name.
ON-ERROR, if non-nil, is called with the error condition instead
of signaling; use this to clean up async state on failure."
  (gdocs-api--request 'post
                      (concat gdocs-api--docs-base-url
                              "/" document-id ":batchUpdate")
                      callback
                      :account account
                      :body (json-encode `((requests . ,requests)))
                      :on-error on-error))

(defun gdocs-api-create-document (title callback &optional account)
  "Create a new Google Docs document with TITLE.
CALLBACK is called with the parsed JSON response, which includes
the documentId.  ACCOUNT is an optional account name."
  (gdocs-api--request 'post
                      gdocs-api--docs-base-url
                      callback
                      :account account
                      :body (json-encode `((title . ,title)))))

;;;; Public API — Google Drive

(defun gdocs-api-get-file-metadata (file-id callback &optional account)
  "Fetch Google Drive metadata for FILE-ID.
CALLBACK is called with the parsed JSON metadata containing id,
name, modifiedTime, version, and headRevisionId.  ACCOUNT is an
optional account name."
  (gdocs-api--request
   'get
   (concat gdocs-api--drive-base-url "/" file-id
           ;; Minimum fields needed for sync state tracking
           "?fields=id,name,modifiedTime,version,headRevisionId")
   callback
   :account account))

(defun gdocs-api-list-files (query callback &optional account page-token)
  "List Google Drive files matching QUERY.
CALLBACK is called with the parsed JSON response containing files
and nextPageToken.  ACCOUNT is an optional account name.
PAGE-TOKEN, if non-nil, is used for pagination."
  (let ((params (gdocs-api--build-list-files-params query page-token)))
    (gdocs-api--request 'get
                        (concat gdocs-api--drive-base-url params)
                        callback
                        :account account)))

(defun gdocs-api--build-list-files-params (query page-token)
  "Build query string for a Drive files.list request.
QUERY is the search query.  PAGE-TOKEN, if non-nil, is appended
for pagination."
  (concat "?q=" (url-hexify-string query)
          ;; Minimum fields for file listing and pagination
          "&fields=files(id,name,mimeType,modifiedTime),nextPageToken"
          (when page-token
            (concat "&pageToken=" (url-hexify-string page-token)))))

(defun gdocs-api-upload-image (file-path callback &optional account folder-id)
  "Upload an image at FILE-PATH to Google Drive.
CALLBACK is called with the parsed JSON file metadata.  ACCOUNT
is an optional account name.  FOLDER-ID, if non-nil, specifies
the parent folder for the uploaded file."
  (let* ((filename (file-name-nondirectory file-path))
         (mime-type (gdocs-api--guess-image-mime-type file-path))
         (metadata (gdocs-api--build-upload-metadata filename folder-id))
         (acct (gdocs-auth--resolve-account account)))
    (gdocs-auth-get-access-token
     acct
     (lambda (token)
       (gdocs-api--send-multipart-upload
        token file-path mime-type metadata callback acct)))))

(defun gdocs-api--build-upload-metadata (filename folder-id)
  "Build the metadata JSON for a Drive upload.
FILENAME is the name of the file.  FOLDER-ID, if non-nil,
specifies the parent folder."
  (let ((meta `((name . ,filename))))
    (when folder-id
      (push `(parents . [,folder-id]) meta))
    (json-encode meta)))

(defun gdocs-api--guess-image-mime-type (file-path)
  "Guess the MIME type for an image at FILE-PATH.
Return a MIME type string based on the file extension."
  (let ((ext (downcase (or (file-name-extension file-path) ""))))
    (pcase ext
      ("png" "image/png")
      ("jpg" "image/jpeg")
      ("jpeg" "image/jpeg")
      ("gif" "image/gif")
      ("webp" "image/webp")
      ("svg" "image/svg+xml")
      (_ (error "Unsupported image type: %s (supported: png, jpg, jpeg, gif, webp, svg)" ext)))))

(defun gdocs-api--send-multipart-upload (token file-path mime-type
                                               metadata callback account)
  "Send a multipart upload request to Google Drive.
TOKEN is the OAuth access token.  FILE-PATH is the local file.
MIME-TYPE is the file's MIME type.  METADATA is the JSON metadata
string.  CALLBACK receives the parsed JSON response.  ACCOUNT is
the account name for error reporting."
  ;; Uploads go through plz directly (not gdocs-api--request) because
  ;; multipart bodies cannot be transparently retried.  On failure,
  ;; the error handler reports but does not retry.
  ;; Generate a unique boundary unlikely to appear in the file content
  (let* ((boundary (format "gdocs-boundary-%s" (sha1 (format "%s%s" file-path (float-time)))))
         (body (gdocs-api--build-multipart-body
                boundary metadata file-path mime-type)))
    (plz 'post gdocs-api--drive-upload-url
      :headers `(("Authorization" . ,(concat "Bearer " token))
                 ("Content-Type" . ,(concat "multipart/related; boundary=" boundary)))
      :body body
      :as #'json-read
      :then callback
      :else (lambda (err)
              ;; Pass max-retries as attempt to prevent retry — multipart
              ;; bodies cannot be transparently replayed.
              (gdocs-api--handle-error
               err 'post gdocs-api--drive-upload-url
               callback account nil gdocs-api--max-retries)))))

(defun gdocs-api--build-multipart-body (boundary metadata file-path mime-type)
  "Build a multipart/related body for a Drive upload.
BOUNDARY is the MIME boundary string.  METADATA is the JSON
metadata part.  FILE-PATH is the path to the file to upload.
MIME-TYPE is the file's MIME type."
  (concat "--" boundary "\r\n"
          "Content-Type: application/json; charset=UTF-8\r\n\r\n"
          metadata "\r\n"
          "--" boundary "\r\n"
          "Content-Type: " mime-type "\r\n"
          "Content-Transfer-Encoding: base64\r\n\r\n"
          (gdocs-api--file-to-base64 file-path) "\r\n"
          "--" boundary "--"))

(defun gdocs-api--file-to-base64 (file-path)
  "Read FILE-PATH and return its contents as a base64 string."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file-path)
    (base64-encode-region (point-min) (point-max) t)
    (buffer-string)))

(defun gdocs-api-rename-file (file-id new-name callback &optional account)
  "Rename the Google Drive file FILE-ID to NEW-NAME.
CALLBACK is called with the parsed JSON response on success.
ACCOUNT is an optional account name."
  (gdocs-api--request
   'patch
   (concat gdocs-api--drive-base-url "/" file-id)
   callback
   :account account
   :body (json-encode `((name . ,new-name)))))

(defun gdocs-api-create-folder (name callback &optional account parent-folder-id)
  "Create a new Google Drive folder named NAME.
CALLBACK is called with the parsed JSON response containing the
folder id.  ACCOUNT is an optional account name.
PARENT-FOLDER-ID, if non-nil, places the folder inside the
specified parent."
  (let ((body `((name . ,name)
                (mimeType . "application/vnd.google-apps.folder"))))
    (when parent-folder-id
      (push `(parents . [,parent-folder-id]) body))
    (gdocs-api--request 'post
                        gdocs-api--drive-base-url
                        callback
                        :account account
                        :body (json-encode body))))

(defun gdocs-api-move-file (file-id folder-id callback &optional account)
  "Move the Google Drive file FILE-ID into FOLDER-ID.
The file is added to FOLDER-ID and removed from the root folder.
CALLBACK is called with the parsed JSON response on success.
ACCOUNT is an optional account name."
  (gdocs-api--request
   'patch
   (concat gdocs-api--drive-base-url "/" file-id
           "?addParents=" (url-hexify-string folder-id)
           "&removeParents=root")
   callback
   :account account))

(defun gdocs-api-search-files (query callback &optional account)
  "Search Google Drive files with full-text QUERY.
CALLBACK is called with the parsed JSON response.  ACCOUNT is an
optional account name."
  (gdocs-api-list-files
   ;; Escape for the Drive API query language: backslashes first (to avoid
   ;; double-escaping), then single quotes.  The multiple backslash layers
   ;; are: Elisp string escaping on top of regexp escaping.
   (format "fullText contains '%s'"
           (replace-regexp-in-string
            "'" "\\\\'"
            (replace-regexp-in-string "\\\\" "\\\\\\\\" query)))
   callback
   account))

;;;; Public API — Drive Comments

(defun gdocs-api-list-comments (file-id callback &optional account)
  "Fetch all comments on FILE-ID via the Drive Comments API.
CALLBACK receives a list of comment alists.  Paginates
automatically with pageSize=100.  ACCOUNT is an optional account
name."
  (gdocs-api--list-comments-page file-id nil nil callback account))

(defun gdocs-api-resolve-comment (file-id comment-id callback
                                           &optional account)
  "Resolve COMMENT-ID on FILE-ID via the Drive Comments API.
Marks the comment as resolved by setting `resolved' to true.
CALLBACK receives the updated comment alist.  ACCOUNT is an
optional account name."
  (let ((url (concat gdocs-api--drive-base-url "/" file-id
                     "/comments/" comment-id
                     "?fields=id,resolved")))
    (gdocs-api--request
     'patch url callback
     :account account
     :body (json-encode '((resolved . t))))))

(defun gdocs-api--list-comments-page (file-id page-token accumulated
                                               callback account)
  "Fetch one page of comments on FILE-ID and continue if more exist.
PAGE-TOKEN paginates; ACCUMULATED collects results across pages.
CALLBACK receives the complete list when done.  ACCOUNT is the
account name."
  (let ((url (concat gdocs-api--drive-base-url "/" file-id "/comments"
                     "?fields="
                     (url-hexify-string
                      (concat "comments(id,content,"
                              "quotedFileContent,resolved,deleted,"
                              "replies(id,content,author,createdTime),"
                              "author,createdTime),nextPageToken"))
                     "&pageSize=100"
                     (when page-token
                       (concat "&pageToken="
                               (url-hexify-string page-token))))))
    (gdocs-api--request
     'get url
     (lambda (json)
       (let* ((page-comments (append (alist-get 'comments json) nil))
              (all (append accumulated page-comments))
              (next (alist-get 'nextPageToken json)))
         (if next
             (gdocs-api--list-comments-page
              file-id next all callback account)
           (funcall callback all))))
     :account account)))

;;;; Core request infrastructure

(cl-defun gdocs-api--request (method url callback
                                     &key account body (attempt 0) on-error)
  "Send an API request with authentication and retry logic.
METHOD is an HTTP method symbol (get, post, etc.).  URL is the
full API endpoint.  CALLBACK is called with the parsed JSON
response on success.  ACCOUNT is the account name for
authentication.  BODY is an optional JSON string for the request
body.  ATTEMPT is the current retry attempt number, used
internally.  ON-ERROR, if non-nil, is called with the error
condition instead of signaling."
  (let ((acct (gdocs-auth--resolve-account account)))
    (gdocs-api--show-progress)
    (gdocs-auth-get-access-token
     acct
     (lambda (token)
       (gdocs-api--send-request method url token callback
                                acct body attempt on-error))
     (lambda (err-msg)
       (gdocs-api--hide-progress)
       (if on-error
           (funcall on-error (cons 'error (list err-msg)))
         (error "%s" err-msg))))))

(defun gdocs-api--send-request (method url token callback account body attempt
                                       &optional on-error)
  "Send the actual HTTP request via plz.
METHOD is an HTTP method symbol.  URL is the API endpoint.
TOKEN is the OAuth access token string.  CALLBACK is called with
parsed JSON on success.  ACCOUNT is the account name for
re-authentication errors.  BODY is an optional JSON string.
ATTEMPT is the current retry count.  ON-ERROR, if non-nil, is
called with the error condition instead of signaling."
  (let ((headers (gdocs-api--build-headers token body)))
    (apply #'plz method url
           :headers headers
           :as #'json-read
           :then (lambda (json)
                   (gdocs-api--hide-progress)
                   (funcall callback json))
           :else (lambda (err)
                   (gdocs-api--hide-progress)
                   (condition-case api-err
                       (gdocs-api--handle-error
                        err method url callback account body attempt
                        on-error)
                     (error
                      (if on-error
                          (funcall on-error api-err)
                        (signal (car api-err) (cdr api-err))))))
           ;; Conditionally append :body via apply's final-arg spreading
           (when body (list :body body)))))

(defun gdocs-api--build-headers (token body)
  "Build HTTP headers with TOKEN for authorization.
BODY, if non-nil, causes a Content-Type header to be included."
  (let ((headers `(("Authorization" . ,(concat "Bearer " token)))))
    (when body
      (push '("Content-Type" . "application/json") headers))
    headers))

;;;; Error handling

(defun gdocs-api--handle-error (err method url callback account body attempt
                                    &optional on-error)
  "Handle an API error, retrying on transient failures.
ERR is the `plz-error' structure.  METHOD, URL, CALLBACK,
ACCOUNT, BODY, and ATTEMPT are from the original request, used
for retries.  ON-ERROR is propagated through retries."
  (let ((status (gdocs-api--extract-http-status err)))
    (cond
     ((and status (gdocs-api--retryable-status-p status)
           (< attempt gdocs-api--max-retries))
      (gdocs-api--schedule-retry
       err method url callback account body attempt on-error))
     ((and status (memq status '(401 403)))
      (let ((msg (format "Authentication failed (HTTP %d) for account %s; run `gdocs-authenticate' to re-authorize"
                         status account)))
        (if on-error
            (funcall on-error (cons 'error (list msg)))
          (error "%s" msg))))
     (t
      (let ((msg (format "Google API request failed: %s"
                         (gdocs-api--format-error err))))
        (if on-error
            (funcall on-error (cons 'error (list msg)))
          (error "%s" msg)))))))

(defun gdocs-api--schedule-retry (err method url callback account body attempt
                                      &optional on-error)
  "Schedule a retry after an appropriate backoff delay.
ERR is the error from the failed request.  METHOD, URL, CALLBACK,
ACCOUNT, BODY, and ATTEMPT are from the original request.
ON-ERROR is propagated to the retried request."
  (let ((delay (gdocs-api--retry-delay err attempt)))
    (message "Google API request failed (attempt %d/%d), retrying in %ds..."
             (1+ attempt) gdocs-api--max-retries delay)
    (run-at-time delay nil
                 #'gdocs-api--request
                 method url callback
                 :account account
                 :body body
                 :attempt (1+ attempt)
                 :on-error on-error)))

(defun gdocs-api--extract-http-status (err)
  "Extract the HTTP status code from a `plz-error' ERR.
Return the status code as an integer, or nil if the error is not
an HTTP error."
  (when-let* ((response (plz-error-response err)))
    (plz-response-status response)))

(defun gdocs-api--retryable-status-p (status)
  "Return non-nil if STATUS is a retryable HTTP status code."
  (memq status gdocs-api--retryable-statuses))

(defun gdocs-api--retry-delay (err attempt)
  "Compute the retry delay in seconds for ATTEMPT.
ERR is the `plz-error' from the failed request.  If the response
includes a Retry-After header, use that value.  Otherwise, use
exponential backoff: 2^ATTEMPT seconds, capped at
`gdocs-api--max-backoff'."
  (or (gdocs-api--extract-retry-after err)
      (min (expt 2 attempt) gdocs-api--max-backoff)))

(defun gdocs-api--extract-retry-after (err)
  "Extract the Retry-After header value from a `plz-error' ERR.
Return the value as a positive integer, or nil if the header is
absent or contains an HTTP-date instead of seconds."
  (when-let* ((response (plz-error-response err))
              (headers (plz-response-headers response))
              (retry-after (alist-get 'retry-after headers))
              (seconds (string-to-number retry-after)))
    (when (> seconds 0) seconds)))

(defun gdocs-api--format-error (err)
  "Format a `plz-error' ERR into a human-readable string."
  (or (plz-error-message err)
      (when-let* ((response (plz-error-response err)))
        (format "HTTP %d: %s"
                (plz-response-status response)
                (plz-response-body response)))
      "unknown error"))

;;;; Progress indication

;; Safe because Emacs async callbacks are serialized on the main thread.
(defvar gdocs-api--active-requests 0
  "Count of currently active API requests.")

(defvar gdocs-api--modeline-string ""
  "String shown in the global mode line during API requests.")

(defun gdocs-api--init-modeline ()
  "Register `gdocs-api--modeline-string' in `global-mode-string'."
  (unless (memq 'gdocs-api--modeline-string global-mode-string)
    (or global-mode-string (setq global-mode-string '("")))
    (setq global-mode-string
          (append global-mode-string '(gdocs-api--modeline-string)))))

;; Register at load time so the modeline segment is available
;; before any API call; gdocs-mode activation is not required.
(gdocs-api--init-modeline)

(defun gdocs-api--show-progress ()
  "Increment the active request count and update the modeline."
  (cl-incf gdocs-api--active-requests)
  (gdocs-api--update-modeline))

(defun gdocs-api--hide-progress ()
  "Decrement the active request count and update the modeline."
  (cl-decf gdocs-api--active-requests)
  ;; Guard against underflow from orphaned hide-progress calls
  (when (< gdocs-api--active-requests 0)
    (setq gdocs-api--active-requests 0))
  (gdocs-api--update-modeline))

(defun gdocs-api--update-modeline ()
  "Update the modeline to reflect active API request status."
  (setq gdocs-api--modeline-string
        (if (> gdocs-api--active-requests 0)
            " [GDocs: syncing...]"
          ""))
  (force-mode-line-update t))

(provide 'gdocs-api)
;;; gdocs-api.el ends here
