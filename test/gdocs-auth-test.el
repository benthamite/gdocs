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

(ert-deftest gdocs-auth-test-resolve-account-single ()
  "With one account, resolve-account auto-selects it without prompting."
  (let ((gdocs-accounts '(("solo" . ((client-id . "id")
                                      (client-secret . "secret"))))))
    (should (equal "solo" (gdocs-auth--resolve-account nil)))))

(ert-deftest gdocs-auth-test-resolve-account-explicit ()
  "With an explicit ACCOUNT argument, return it directly."
  (let ((gdocs-accounts '(("personal" . ((client-id . "id1")
                                          (client-secret . "s1")))
                           ("work" . ((client-id . "id2")
                                      (client-secret . "s2"))))))
    (should (equal "work" (gdocs-auth--resolve-account "work")))))

;;;; Error on missing config

(ert-deftest gdocs-auth-test-resolve-account-nil ()
  "With no accounts configured, signal an error."
  (let ((gdocs-accounts nil))
    (should-error (gdocs-auth--resolve-account nil)
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

(provide 'gdocs-auth-test)
;;; gdocs-auth-test.el ends here
