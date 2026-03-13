;;; gdocs-api-test.el --- Tests for gdocs-api -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Pablo Stafforini

;; Author: Pablo Stafforini

;; This file is not part of GNU Emacs.

;;; Commentary:

;; ERT tests for the gdocs-api module: header building, error handling,
;; retry logic, progress tracking, MIME type guessing, multipart body
;; construction, and URL/metadata building.

;;; Code:

(require 'ert)
(require 'gdocs-api)
(require 'gdocs-test-helpers)
(require 'cl-lib)
(require 'plz)

;;;; Header building

(ert-deftest gdocs-api-test-build-headers-get-request ()
  "GET requests (no body) produce only the Authorization header."
  (let ((headers (gdocs-api--build-headers "my-token" nil)))
    (should (equal 1 (length headers)))
    (should (equal "Bearer my-token"
                   (cdr (assoc "Authorization" headers))))))

(ert-deftest gdocs-api-test-build-headers-post-request ()
  "POST requests (with body) include both Authorization and Content-Type."
  (let ((headers (gdocs-api--build-headers "my-token" "{\"key\":\"val\"}")))
    (should (equal 2 (length headers)))
    (should (equal "Bearer my-token"
                   (cdr (assoc "Authorization" headers))))
    (should (equal "application/json"
                   (cdr (assoc "Content-Type" headers))))))

;;;; Error handling — retryable status codes

(ert-deftest gdocs-api-test-retryable-status-429 ()
  "HTTP 429 (Too Many Requests) is retryable."
  (should (gdocs-api--retryable-status-p 429)))

(ert-deftest gdocs-api-test-retryable-status-500 ()
  "HTTP 500 (Internal Server Error) is retryable."
  (should (gdocs-api--retryable-status-p 500)))

(ert-deftest gdocs-api-test-retryable-status-503 ()
  "HTTP 503 (Service Unavailable) is retryable."
  (should (gdocs-api--retryable-status-p 503)))

(ert-deftest gdocs-api-test-non-retryable-status-400 ()
  "HTTP 400 (Bad Request) is not retryable."
  (should-not (gdocs-api--retryable-status-p 400)))

(ert-deftest gdocs-api-test-non-retryable-status-404 ()
  "HTTP 404 (Not Found) is not retryable."
  (should-not (gdocs-api--retryable-status-p 404)))

(ert-deftest gdocs-api-test-non-retryable-status-401 ()
  "HTTP 401 (Unauthorized) is not retryable."
  (should-not (gdocs-api--retryable-status-p 401)))

(ert-deftest gdocs-api-test-non-retryable-status-403 ()
  "HTTP 403 (Forbidden) is not retryable."
  (should-not (gdocs-api--retryable-status-p 403)))

;;;; Error handling — extract HTTP status

(ert-deftest gdocs-api-test-extract-http-status ()
  "Extract the HTTP status code from a plz-error with a response."
  (let ((err (make-plz-error
              :message "HTTP error"
              :response (make-plz-response :status 429 :headers nil :body ""))))
    (should (equal 429 (gdocs-api--extract-http-status err)))))

(ert-deftest gdocs-api-test-extract-http-status-nil-when-no-response ()
  "Return nil when the plz-error has no response (e.g. network error)."
  (let ((err (make-plz-error :message "Connection refused" :response nil)))
    (should-not (gdocs-api--extract-http-status err))))

;;;; Error handling — auth failures

(ert-deftest gdocs-api-test-handle-error-401-signals-auth-error ()
  "HTTP 401 signals an error mentioning re-authorization."
  (let ((err (make-plz-error
              :message "Unauthorized"
              :response (make-plz-response :status 401 :headers nil :body ""))))
    (let ((error-data (should-error
                       (gdocs-api--handle-error
                        err 'get "https://example.com" #'ignore "my-acct" nil 0)
                       :type 'error)))
      (should (string-match-p "Authentication failed" (cadr error-data)))
      (should (string-match-p "401" (cadr error-data)))
      (should (string-match-p "gdocs-authenticate" (cadr error-data))))))

(ert-deftest gdocs-api-test-handle-error-403-signals-auth-error ()
  "HTTP 403 signals an error mentioning re-authorization."
  (let ((err (make-plz-error
              :message "Forbidden"
              :response (make-plz-response :status 403 :headers nil :body ""))))
    (let ((error-data (should-error
                       (gdocs-api--handle-error
                        err 'get "https://example.com" #'ignore "work" nil 0)
                       :type 'error)))
      (should (string-match-p "Authentication failed" (cadr error-data)))
      (should (string-match-p "403" (cadr error-data))))))

;;;; Error handling — max retries exceeded

(ert-deftest gdocs-api-test-handle-error-signals-after-max-retries ()
  "After max retries, a retryable error is signaled rather than retried."
  (let ((err (make-plz-error
              :message "Service Unavailable"
              :response (make-plz-response :status 503 :headers nil :body ""))))
    (should-error
     (gdocs-api--handle-error
      err 'get "https://example.com" #'ignore "acct" nil gdocs-api--max-retries)
     :type 'error)))

;;;; Error handling — non-retryable non-auth error

(ert-deftest gdocs-api-test-handle-error-400-signals-immediately ()
  "HTTP 400 is not retryable and signals immediately."
  (let ((err (make-plz-error
              :message "Bad Request"
              :response (make-plz-response :status 400 :headers nil
                                           :body "invalid JSON"))))
    (let ((error-data (should-error
                       (gdocs-api--handle-error
                        err 'post "https://example.com" #'ignore "acct" nil 0)
                       :type 'error)))
      (should (string-match-p "Google API request failed" (cadr error-data))))))

;;;; Error handling — format error

(ert-deftest gdocs-api-test-format-error-with-message ()
  "Format error uses the error message when available."
  (let ((err (make-plz-error :message "Connection timeout" :response nil)))
    (should (equal "Connection timeout" (gdocs-api--format-error err)))))

(ert-deftest gdocs-api-test-format-error-with-response ()
  "Format error falls back to HTTP status and body when no message."
  (let ((err (make-plz-error
              :message nil
              :response (make-plz-response :status 404 :headers nil
                                           :body "Not Found"))))
    (should (equal "HTTP 404: Not Found" (gdocs-api--format-error err)))))

(ert-deftest gdocs-api-test-format-error-unknown ()
  "Format error returns 'unknown error' when neither message nor response."
  (let ((err (make-plz-error :message nil :response nil)))
    (should (equal "unknown error" (gdocs-api--format-error err)))))

;;;; Retry delay computation

(ert-deftest gdocs-api-test-retry-delay-exponential-backoff-attempt-0 ()
  "Attempt 0 has a 1-second delay (2^0)."
  (let ((err (make-plz-error
              :message "err"
              :response (make-plz-response :status 429 :headers nil :body ""))))
    (should (equal 1 (gdocs-api--retry-delay err 0)))))

(ert-deftest gdocs-api-test-retry-delay-exponential-backoff-attempt-1 ()
  "Attempt 1 has a 2-second delay (2^1)."
  (let ((err (make-plz-error
              :message "err"
              :response (make-plz-response :status 500 :headers nil :body ""))))
    (should (equal 2 (gdocs-api--retry-delay err 1)))))

(ert-deftest gdocs-api-test-retry-delay-exponential-backoff-attempt-2 ()
  "Attempt 2 has a 4-second delay (2^2)."
  (let ((err (make-plz-error
              :message "err"
              :response (make-plz-response :status 503 :headers nil :body ""))))
    (should (equal 4 (gdocs-api--retry-delay err 2)))))

(ert-deftest gdocs-api-test-retry-delay-max-backoff-cap ()
  "Delay is capped at `gdocs-api--max-backoff' for large attempt numbers."
  (let ((err (make-plz-error
              :message "err"
              :response (make-plz-response :status 429 :headers nil :body ""))))
    (should (equal gdocs-api--max-backoff (gdocs-api--retry-delay err 10)))))

(ert-deftest gdocs-api-test-retry-delay-respects-retry-after-header ()
  "Retry-After header takes precedence over exponential backoff."
  (let ((err (make-plz-error
              :message "err"
              :response (make-plz-response
                         :status 429
                         :headers '((retry-after . "15"))
                         :body ""))))
    (should (equal 15 (gdocs-api--retry-delay err 0)))))

;;;; Retry-After extraction

(ert-deftest gdocs-api-test-extract-retry-after-present ()
  "Extract Retry-After header value as integer."
  (let ((err (make-plz-error
              :message "err"
              :response (make-plz-response
                         :status 429
                         :headers '((retry-after . "42"))
                         :body ""))))
    (should (equal 42 (gdocs-api--extract-retry-after err)))))

(ert-deftest gdocs-api-test-extract-retry-after-absent ()
  "Return nil when Retry-After header is absent."
  (let ((err (make-plz-error
              :message "err"
              :response (make-plz-response :status 429 :headers nil :body ""))))
    (should-not (gdocs-api--extract-retry-after err))))

(ert-deftest gdocs-api-test-extract-retry-after-no-response ()
  "Return nil when there is no response at all."
  (let ((err (make-plz-error :message "network error" :response nil)))
    (should-not (gdocs-api--extract-retry-after err))))

;;;; Progress tracking

(ert-deftest gdocs-api-test-show-progress-increments-counter ()
  "Show-progress increments the active request counter."
  (let ((gdocs-api--active-requests 0)
        (gdocs-api--modeline-string ""))
    (gdocs-api--show-progress)
    (should (equal 1 gdocs-api--active-requests))))

(ert-deftest gdocs-api-test-hide-progress-decrements-counter ()
  "Hide-progress decrements the active request counter."
  (let ((gdocs-api--active-requests 2)
        (gdocs-api--modeline-string ""))
    (gdocs-api--hide-progress)
    (should (equal 1 gdocs-api--active-requests))))

(ert-deftest gdocs-api-test-hide-progress-underflow-protection ()
  "Hide-progress clamps to zero rather than going negative."
  (let ((gdocs-api--active-requests 0)
        (gdocs-api--modeline-string ""))
    (gdocs-api--hide-progress)
    (should (equal 0 gdocs-api--active-requests))))

(ert-deftest gdocs-api-test-modeline-shows-syncing-when-active ()
  "Modeline shows syncing indicator when requests are active."
  (let ((gdocs-api--active-requests 0)
        (gdocs-api--modeline-string ""))
    (gdocs-api--show-progress)
    (should (string-match-p "syncing" gdocs-api--modeline-string))))

(ert-deftest gdocs-api-test-modeline-empty-when-done ()
  "Modeline is empty when no requests are active."
  (let ((gdocs-api--active-requests 1)
        (gdocs-api--modeline-string " [GDocs: syncing...]"))
    (gdocs-api--hide-progress)
    (should (equal "" gdocs-api--modeline-string))))

;;;; MIME type guessing

(ert-deftest gdocs-api-test-mime-type-png ()
  "PNG files produce image/png."
  (should (equal "image/png" (gdocs-api--guess-image-mime-type "photo.png"))))

(ert-deftest gdocs-api-test-mime-type-jpg ()
  "JPG files produce image/jpeg."
  (should (equal "image/jpeg" (gdocs-api--guess-image-mime-type "photo.jpg"))))

(ert-deftest gdocs-api-test-mime-type-jpeg ()
  "JPEG files produce image/jpeg."
  (should (equal "image/jpeg" (gdocs-api--guess-image-mime-type "photo.jpeg"))))

(ert-deftest gdocs-api-test-mime-type-gif ()
  "GIF files produce image/gif."
  (should (equal "image/gif" (gdocs-api--guess-image-mime-type "anim.gif"))))

(ert-deftest gdocs-api-test-mime-type-webp ()
  "WebP files produce image/webp."
  (should (equal "image/webp" (gdocs-api--guess-image-mime-type "modern.webp"))))

(ert-deftest gdocs-api-test-mime-type-svg ()
  "SVG files produce image/svg+xml."
  (should (equal "image/svg+xml" (gdocs-api--guess-image-mime-type "icon.svg"))))

(ert-deftest gdocs-api-test-mime-type-case-insensitive ()
  "MIME type guessing is case-insensitive."
  (should (equal "image/png" (gdocs-api--guess-image-mime-type "PHOTO.PNG"))))

(ert-deftest gdocs-api-test-mime-type-unknown-signals-error ()
  "Unknown image extension signals an error."
  (should-error (gdocs-api--guess-image-mime-type "document.bmp")
                :type 'error))

(ert-deftest gdocs-api-test-mime-type-no-extension-signals-error ()
  "A file with no extension signals an error."
  (should-error (gdocs-api--guess-image-mime-type "noextension")
                :type 'error))

;;;; Multipart body building

(ert-deftest gdocs-api-test-multipart-body-contains-boundary-separators ()
  "The multipart body contains the boundary as part separators."
  (let* ((temp-file (make-temp-file "gdocs-test-img" nil ".png"))
         (boundary "test-boundary-123")
         (metadata "{\"name\":\"test.png\"}")
         (body (unwind-protect
                   (progn
                     (with-temp-file temp-file
                       (insert "fake-png-data"))
                     (gdocs-api--build-multipart-body
                      boundary metadata temp-file "image/png"))
                 (delete-file temp-file))))
    (should (string-match-p (regexp-quote (concat "--" boundary)) body))))

(ert-deftest gdocs-api-test-multipart-body-contains-metadata-json ()
  "The multipart body contains the metadata JSON."
  (let* ((temp-file (make-temp-file "gdocs-test-img" nil ".png"))
         (boundary "test-boundary-456")
         (metadata "{\"name\":\"photo.png\"}")
         (body (unwind-protect
                   (progn
                     (with-temp-file temp-file
                       (insert "fake-png-data"))
                     (gdocs-api--build-multipart-body
                      boundary metadata temp-file "image/png"))
                 (delete-file temp-file))))
    (should (string-match-p (regexp-quote metadata) body))))

(ert-deftest gdocs-api-test-multipart-body-contains-image-content-type ()
  "The multipart body contains the Content-Type header for the image."
  (let* ((temp-file (make-temp-file "gdocs-test-img" nil ".gif"))
         (boundary "test-boundary-789")
         (metadata "{\"name\":\"anim.gif\"}")
         (body (unwind-protect
                   (progn
                     (with-temp-file temp-file
                       (insert "fake-gif-data"))
                     (gdocs-api--build-multipart-body
                      boundary metadata temp-file "image/gif"))
                 (delete-file temp-file))))
    (should (string-match-p "Content-Type: image/gif" body))))

(ert-deftest gdocs-api-test-multipart-body-ends-with-terminator ()
  "The multipart body ends with the boundary terminator (--boundary--)."
  (let* ((temp-file (make-temp-file "gdocs-test-img" nil ".png"))
         (boundary "test-boundary-end")
         (metadata "{\"name\":\"test.png\"}")
         (body (unwind-protect
                   (progn
                     (with-temp-file temp-file
                       (insert "fake-png-data"))
                     (gdocs-api--build-multipart-body
                      boundary metadata temp-file "image/png"))
                 (delete-file temp-file))))
    (should (string-match-p
             (regexp-quote (concat "--" boundary "--"))
             body))))

;;;; URL parameter building

(ert-deftest gdocs-api-test-build-list-files-params-without-page-token ()
  "Without a page token, params contain the query and fields only."
  (let ((params (gdocs-api--build-list-files-params "mimeType='application/vnd.google-apps.document'" nil)))
    (should (string-match-p "^\\?q=" params))
    (should (string-match-p "fields=" params))
    ;; No &pageToken= parameter (the word "nextPageToken" appears in the
    ;; fields list, so we check specifically for the parameter form)
    (should-not (string-match-p "&pageToken=" params))))

(ert-deftest gdocs-api-test-build-list-files-params-with-page-token ()
  "With a page token, params include the pageToken parameter."
  (let ((params (gdocs-api--build-list-files-params "mimeType='application/vnd.google-apps.document'" "abc123")))
    (should (string-match-p "pageToken=" params))
    (should (string-match-p "abc123" params))))

(ert-deftest gdocs-api-test-build-list-files-params-query-encoded ()
  "The query string is URL-encoded."
  (let ((params (gdocs-api--build-list-files-params "name = 'my file'" nil)))
    ;; Spaces and single quotes in the query should be percent-encoded
    (should-not (string-match-p "name = 'my file'" params))
    (should (string-match-p "q=" params))))

;;;; Upload metadata building

(ert-deftest gdocs-api-test-build-upload-metadata-without-folder ()
  "Without a folder ID, metadata contains only the name."
  (let* ((json-str (gdocs-api--build-upload-metadata "photo.png" nil))
         (parsed (json-read-from-string json-str)))
    (should (equal "photo.png" (alist-get 'name parsed)))
    (should-not (alist-get 'parents parsed))))

(ert-deftest gdocs-api-test-build-upload-metadata-with-folder ()
  "With a folder ID, metadata includes the parents array."
  (let* ((json-str (gdocs-api--build-upload-metadata "photo.png" "folder-xyz"))
         (parsed (json-read-from-string json-str)))
    (should (equal "photo.png" (alist-get 'name parsed)))
    (let ((parents (alist-get 'parents parsed)))
      (should parents)
      (should (equal "folder-xyz" (aref parents 0))))))

(provide 'gdocs-api-test)
;;; gdocs-api-test.el ends here
