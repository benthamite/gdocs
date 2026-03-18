;;; gdocs-auth-test.el --- Tests for gdocs-auth -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Pablo Stafforini

;; Author: Pablo Stafforini

;; This file is not part of GNU Emacs.

;;; Commentary:

;; ERT tests for the gdocs-auth module: token loading, refresh, file
;; permissions, multi-account selection, and error conditions.

;;; Code:

(require 'ert)
(require 'gdocs-auth)
(require 'gdocs-test-helpers)
(require 'cl-lib)

;;;; Token loading

(ert-deftest gdocs-auth-test-load-valid-token ()
  "Loading a valid token file returns the access token via callback."
  (gdocs-test-with-temp-dir
    (gdocs-test-write-token-file "test-account")
    (let ((result nil))
      (gdocs-auth-get-access-token
       "test-account"
       (lambda (token) (setq result token)))
      (should (equal result "test-access-token-12345")))))

;;;; Token refresh

(ert-deftest gdocs-auth-test-refresh-expired-token ()
  "An expired token triggers a refresh request with correct parameters."
  (gdocs-test-with-temp-dir
    (gdocs-test-write-token-file
     "test-account"
     `((expires_at . ,(- (float-time) 100))))
    (let ((captured-body nil)
          (result nil))
      (cl-letf (((symbol-function 'plz)
                 (lambda (_method _url &rest args)
                   (setq captured-body (plist-get args :body))
                   (funcall (plist-get args :then)
                            '((access_token . "refreshed-token")
                              (expires_in . 3600))))))
        (gdocs-auth-get-access-token
         "test-account"
         (lambda (token) (setq result token))))
      (should (equal result "refreshed-token"))
      (should (string-match-p "grant_type=refresh_token" captured-body))
      (should (string-match-p "refresh_token=test-refresh-token-67890"
                              captured-body)))))

;;;; Token file permissions

(ert-deftest gdocs-auth-test-token-file-permissions ()
  "Token files are created with 600 permissions."
  (gdocs-test-with-temp-dir
    (let ((path (gdocs-test-write-token-file "test-account")))
      (should (= #o600 (file-modes path))))))

;;;; Multi-account selection

(ert-deftest gdocs-auth-test-select-single-account ()
  "With one account, `gdocs-auth-select-account' returns it directly."
  (let ((gdocs-accounts '(("solo" . ((client-id . "id")
                                      (client-secret . "secret"))))))
    (should (equal "solo" (gdocs-auth-select-account)))))

(ert-deftest gdocs-auth-test-select-multiple-accounts ()
  "With multiple accounts, `gdocs-auth-select-account' prompts the user."
  (let ((gdocs-accounts '(("personal" . ((client-id . "id1")
                                          (client-secret . "s1")))
                           ("work" . ((client-id . "id2")
                                      (client-secret . "s2")))))
        (prompted nil))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt collection &rest _args)
                 (setq prompted t)
                 (car collection))))
      (gdocs-auth-select-account)
      (should prompted))))

(ert-deftest gdocs-auth-test-resolve-account-passthrough ()
  "With an explicit ACCOUNT argument, resolve-account returns it directly."
  (let ((gdocs-accounts '(("personal" . ((client-id . "id1")
                                          (client-secret . "s1"))))))
    (should (equal "work" (gdocs-auth--resolve-account "work")))))

;;;; Error on missing config

(ert-deftest gdocs-auth-test-error-when-no-accounts ()
  "With no accounts configured, signal an error."
  (let ((gdocs-accounts nil))
    (should-error (gdocs-auth-select-account)
                  :type 'error)))

;;;; Token file path

(ert-deftest gdocs-auth-test-token-file-path ()
  "Token file path is DIRECTORY/ACCOUNT.json."
  (let ((gdocs-token-directory "/tmp/gdocs-tokens/"))
    (should (equal "/tmp/gdocs-tokens/myaccount.json"
                   (gdocs-auth--token-file-path "myaccount")))))

;;;; Token validity

(ert-deftest gdocs-auth-test-token-needs-refresh ()
  "A token expiring within the margin is considered invalid."
  (let ((token-data `((access_token . "old-token")
                      (expires_at . ,(+ (float-time) 30)))))
    (should-not (gdocs-auth--token-valid-p token-data))))

(ert-deftest gdocs-auth-test-token-valid ()
  "A token with an expiry well in the future is valid."
  (let ((token-data `((access_token . "good-token")
                      (expires_at . ,(+ (float-time) 3600)))))
    (should (gdocs-auth--token-valid-p token-data))))

;;;; Token persistence round-trip

(ert-deftest gdocs-auth-test-save-and-load-tokens ()
  "Round-trip: writing then reading a token file preserves data."
  (gdocs-test-with-temp-dir
    (let ((data `((access_token . "test-access-token")
                  (refresh_token . "test-refresh-token")
                  (expires_at . 1700000000)
                  (token_type . "Bearer")
                  (client_id . "cid")
                  (client_secret . "csec"))))
      (gdocs-auth--write-token-file "roundtrip" data)
      (let ((loaded (gdocs-auth--read-token-file-if-exists "roundtrip")))
        (should (equal "test-access-token"
                       (alist-get 'access_token loaded)))
        (should (equal "test-refresh-token"
                       (alist-get 'refresh_token loaded)))
        (should (equal 1700000000
                       (alist-get 'expires_at loaded)))
        (should (equal "Bearer"
                       (alist-get 'token_type loaded)))))))

;;;; Nonexistent token file returns nil

(ert-deftest gdocs-auth-test-read-nonexistent-returns-nil ()
  "Reading a token file that does not exist returns nil."
  (gdocs-test-with-temp-dir
    (should-not (gdocs-auth--read-token-file-if-exists "no-such-account"))))

;;;; Auth code extraction

(ert-deftest gdocs-auth-test-extract-auth-code-valid ()
  "extract-auth-code extracts the code from a standard OAuth GET request."
  (let ((request "GET /?code=4/0AbCdEf HTTP/1.1\r\nHost: localhost\r\n\r\n"))
    (should (equal "4/0AbCdEf"
                   (gdocs-auth--extract-auth-code request)))))

(ert-deftest gdocs-auth-test-extract-auth-code-no-code ()
  "extract-auth-code returns nil for a request without a code parameter."
  (let ((request "GET /favicon.ico HTTP/1.1\r\n\r\n"))
    (should-not (gdocs-auth--extract-auth-code request))))

(ert-deftest gdocs-auth-test-extract-auth-code-with-other-params ()
  "extract-auth-code extracts the code when other query params are present."
  (let ((request "GET /?scope=foo&code=ABC123&other=bar HTTP/1.1\r\n\r\n"))
    (should (equal "ABC123"
                   (gdocs-auth--extract-auth-code request)))))

;;;; Token data construction

(ert-deftest gdocs-auth-test-build-token-data-fields ()
  "build-token-data includes all 6 expected fields with expires_at in the future."
  (let* ((response '((access_token . "at-123")
                     (refresh_token . "rt-456")
                     (expires_in . 3600)))
         (data (gdocs-auth--build-token-data response "cid" "csec")))
    (should (equal "at-123" (alist-get 'access_token data)))
    (should (equal "rt-456" (alist-get 'refresh_token data)))
    (should (equal "Bearer" (alist-get 'token_type data)))
    (should (equal "cid" (alist-get 'client_id data)))
    (should (equal "csec" (alist-get 'client_secret data)))
    (should (> (alist-get 'expires_at data) (float-time)))))

;;;; Token merge

(ert-deftest gdocs-auth-test-merge-refreshed-token-preserves-refresh-token ()
  "merge-refreshed-token preserves the old refresh_token while updating access_token."
  (let* ((old-data '((access_token . "old-at")
                     (refresh_token . "keep-this-rt")
                     (expires_at . 1000)
                     (token_type . "Bearer")
                     (client_id . "cid")
                     (client_secret . "csec")))
         (response '((access_token . "new-at")
                     (expires_in . 3600)))
         (merged (gdocs-auth--merge-refreshed-token old-data response)))
    (should (equal "new-at" (alist-get 'access_token merged)))
    (should (equal "keep-this-rt" (alist-get 'refresh_token merged)))
    (should (> (alist-get 'expires_at merged) (float-time)))))

(ert-deftest gdocs-auth-test-merge-refreshed-token-preserves-credentials ()
  "merge-refreshed-token preserves client_id and client_secret from old data."
  (let* ((old-data '((access_token . "old-at")
                     (refresh_token . "rt")
                     (expires_at . 1000)
                     (token_type . "Bearer")
                     (client_id . "my-client-id")
                     (client_secret . "my-client-secret")))
         (response '((access_token . "new-at")
                     (expires_in . 3600)))
         (merged (gdocs-auth--merge-refreshed-token old-data response)))
    (should (equal "my-client-id" (alist-get 'client_id merged)))
    (should (equal "my-client-secret" (alist-get 'client_secret merged)))))

;;;; Token directory creation

(ert-deftest gdocs-auth-test-token-directory-creation ()
  "write-token-file creates parent directories if they don't exist."
  (let* ((temp-dir (make-temp-file "gdocs-test-" t))
         (nested-dir (expand-file-name "a/b/c/" temp-dir))
         (gdocs-token-directory nested-dir)
         (data '((access_token . "tok"))))
    (unwind-protect
        (progn
          (should-not (file-directory-p nested-dir))
          (gdocs-auth--write-token-file "deep-account" data)
          (should (file-directory-p nested-dir))
          (should (file-exists-p (gdocs-auth--token-file-path "deep-account"))))
      (delete-directory temp-dir t))))

;;;; URL encoding

(ert-deftest gdocs-auth-test-url-encode-params ()
  "url-encode-params correctly encodes special characters in values."
  (let ((result (gdocs-auth--url-encode-params
                 '(("key" . "hello world")
                   ("url" . "http://example.com/?a=1&b=2")))))
    (should (string-match-p "key=hello%20world" result))
    (should (string-match-p "url=http%3A%2F%2Fexample.com%2F%3Fa%3D1%26b%3D2"
                            result))
    (should (string-match-p "&" result))))

(provide 'gdocs-auth-test)
;;; gdocs-auth-test.el ends here
