;;; gdocs-auth.el --- OAuth 2.0 authentication for Google Docs -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Pablo Stafforini

;; Author: Pablo Stafforini

;; This file is not part of GNU Emacs.

;;; Commentary:

;; OAuth 2.0 flow, token management, and multi-account support for the
;; gdocs package.  Users must configure their own OAuth credentials via
;; `gdocs-accounts'.  No built-in credentials are provided.
;;
;; Token files are stored as JSON in `gdocs-token-directory' with 600
;; permissions.  Each account has its own token file.  Tokens are
;; automatically refreshed before API calls when they expire within a
;; 60-second safety margin.
;;
;; The OAuth flow uses a temporary local HTTP callback server on a
;; random high port to receive the authorization code from Google.

;;; Code:

(require 'json)
(require 'plz)
(require 'browse-url)
(require 'url-util)

;;;; Custom group and variables

(defgroup gdocs nil
  "Google Docs integration for Emacs."
  :group 'tools
  :prefix "gdocs-")

(defcustom gdocs-accounts nil
  "Alist of Google accounts for gdocs.
Each entry is (NAME . ((client-id . \"ID\") (client-secret .
\"SECRET\"))) where NAME is a string identifying the account."
  :type '(alist :key-type string
                :value-type (alist :key-type symbol
                                   :value-type string))
  :group 'gdocs)

(defcustom gdocs-token-directory
  (expand-file-name "gdocs/tokens/" user-emacs-directory)
  "Directory for storing OAuth token files.
Each account's tokens are stored as a JSON file named
ACCOUNT-NAME.json with 600 permissions."
  :type 'directory
  :group 'gdocs)

;;;; Constants

(defconst gdocs-auth--scopes
  "https://www.googleapis.com/auth/documents https://www.googleapis.com/auth/drive"
  "OAuth scopes requested by gdocs.")

(defconst gdocs-auth--auth-url
  "https://accounts.google.com/o/oauth2/v2/auth"
  "Google OAuth 2.0 authorization endpoint.")

(defconst gdocs-auth--token-url
  "https://oauth2.googleapis.com/token"
  "Google OAuth 2.0 token endpoint.")

(defconst gdocs-auth--token-expiry-margin 60
  "Seconds before actual expiry to consider a token expired.
Provides a safety margin for network latency and clock skew
so API calls don't fail mid-flight with an expired token.")

;;;; Public API

(defun gdocs-auth-select-account (&optional prompt)
  "Select a Google account from `gdocs-accounts'.
PROMPT is the string to display when multiple accounts are
configured.  If only one account exists, return its name without
prompting.  Signal an error if `gdocs-accounts' is nil.
See also `gdocs-auth--resolve-account' which additionally accepts
an already-known ACCOUNT argument."
  (gdocs-auth--validate-accounts-configured)
  (if (= 1 (length gdocs-accounts))
      (caar gdocs-accounts)
    (completing-read (or prompt "Google account: ")
                     (mapcar #'car gdocs-accounts)
                     nil t)))

(defun gdocs-auth-get-access-token (account callback)
  "Ensure a valid access token for ACCOUNT, then call CALLBACK with it.
ACCOUNT is a string naming an entry in `gdocs-accounts'.
CALLBACK is a function called with a single argument: the access
token string.  If the token is expired or expiring within
`gdocs-auth--token-expiry-margin' seconds, it is refreshed
asynchronously before calling CALLBACK.  If no token file exists,
initiate the OAuth flow and call CALLBACK upon completion."
  (let ((token-data (gdocs-auth--read-token-file-if-exists account)))
    (cond
     ((null token-data)
      (gdocs-auth--start-oauth-flow account callback))
     ((gdocs-auth--token-valid-p token-data)
      (funcall callback (alist-get 'access_token token-data)))
     (t
      (gdocs-auth--refresh-token account token-data callback)))))

;;;###autoload
(defun gdocs-authenticate (&optional account)
  "Initiate the OAuth flow for ACCOUNT.
If ACCOUNT is nil and multiple accounts are configured, prompt
the user to select one.  If only one account exists, use it
directly."
  (interactive)
  (let ((acct (gdocs-auth--resolve-account account)))
    (gdocs-auth--start-oauth-flow acct nil)))

;;;###autoload
(defun gdocs-logout (&optional account)
  "Delete the stored token file for ACCOUNT.
If ACCOUNT is nil, prompt the user to select one."
  (interactive)
  (let* ((acct (gdocs-auth--resolve-account account))
         (token-file (gdocs-auth--token-file-path acct)))
    (when (file-exists-p token-file)
      (delete-file token-file)
      (message "Logged out of account %s" acct))))

;;;; Account resolution

(defun gdocs-auth--resolve-account (account)
  "Resolve ACCOUNT to a concrete account name string.
If ACCOUNT is non-nil, return it.  If nil and only one account
exists in `gdocs-accounts', return that account's name.  If nil
and multiple accounts exist, prompt with `completing-read'.
Signal an error if `gdocs-accounts' is nil.
See also `gdocs-auth-select-account' for the interactive-only
variant that always prompts when multiple accounts exist."
  (gdocs-auth--validate-accounts-configured)
  (cond
   (account account)
   ((= 1 (length gdocs-accounts))
    (caar gdocs-accounts))
   (t
    (completing-read "Google account: "
                     (mapcar #'car gdocs-accounts)
                     nil t))))

(defun gdocs-auth--validate-accounts-configured ()
  "Check that `gdocs-accounts' is configured.
If not, open the setup guide and signal a user error."
  (unless gdocs-accounts
    (gdocs-auth--open-account-setup-guide)
    (user-error "Please configure `gdocs-accounts' first (see manual)")))

(defun gdocs-auth--open-account-setup-guide ()
  "Open the Info manual to the credential setup guide."
  (info "(gdocs) Obtaining credentials"))

;;;; Token file operations

(defun gdocs-auth--token-file-path (account)
  "Return the token file path for ACCOUNT."
  (expand-file-name (concat account ".json") gdocs-token-directory))

(defun gdocs-auth--read-token-file-if-exists (account)
  "Read and parse the token file for ACCOUNT, or return nil.
Return the parsed JSON as an alist if the file exists, nil
otherwise."
  (let ((path (gdocs-auth--token-file-path account)))
    (when (file-exists-p path)
      (json-read-file path))))

(defun gdocs-auth--write-token-file (account data)
  "Write token DATA to the token file for ACCOUNT.
DATA is an alist that will be serialized as JSON.  The file is
created with 600 permissions inside a 700 directory.  Parent
directories are created as needed."
  (let ((path (gdocs-auth--token-file-path account))
        (dir (file-name-directory (gdocs-auth--token-file-path account))))
    (make-directory dir t)
    (set-file-modes dir #o700)
    ;; Create the file with restrictive permissions from the start to
    ;; avoid a window where secrets are world-readable.
    (let ((old-modes (default-file-modes)))
      (unwind-protect
          (progn
            (set-default-file-modes #o600)
            (with-temp-file path
              (insert (json-encode data))))
        (set-default-file-modes old-modes)))))

;;;; Token validation and refresh

(defun gdocs-auth--token-valid-p (token-data)
  "Return non-nil if TOKEN-DATA contains a non-expired access token.
A token is considered expired if the current time plus
`gdocs-auth--token-expiry-margin' exceeds the stored
expires_at timestamp."
  (let ((expires-at (alist-get 'expires_at token-data)))
    (and expires-at
         (< (+ (float-time) gdocs-auth--token-expiry-margin)
            expires-at))))

(defun gdocs-auth--refresh-token (account token-data callback)
  "Refresh the access token for ACCOUNT using TOKEN-DATA.
TOKEN-DATA must contain a refresh_token.  On success, write the
updated token file and call CALLBACK with the new access token.
CALLBACK is a function receiving one string argument."
  (let ((refresh-token (alist-get 'refresh_token token-data))
        (client-id (alist-get 'client_id token-data))
        (client-secret (alist-get 'client_secret token-data)))
    (unless refresh-token
      (error "No refresh token for account %s; run `gdocs-authenticate'"
             account))
    (plz 'post gdocs-auth--token-url
      :headers '(("Content-Type" . "application/x-www-form-urlencoded"))
      :body (gdocs-auth--url-encode-params
             `(("grant_type" . "refresh_token")
               ("refresh_token" . ,refresh-token)
               ("client_id" . ,client-id)
               ("client_secret" . ,client-secret)))
      :as #'json-read
      :then (lambda (response)
              (let ((new-data (gdocs-auth--merge-refreshed-token
                               token-data response)))
                (gdocs-auth--write-token-file account new-data)
                (funcall callback (alist-get 'access_token new-data))))
      :else (lambda (err)
              (error "Failed to refresh token for account %s: %s"
                     account (plz-error-message err))))))

(defun gdocs-auth--merge-refreshed-token (old-data response)
  "Merge RESPONSE from a token refresh into OLD-DATA.
RESPONSE is the parsed JSON from Google's token endpoint.
OLD-DATA is the existing token alist.  Return a new alist with
the updated access token, expiry, and preserved refresh token."
  (let ((access-token (alist-get 'access_token response))
        (expires-in (alist-get 'expires_in response)))
    `((access_token . ,access-token)
      (refresh_token . ,(alist-get 'refresh_token old-data))
      (expires_at . ,(+ (float-time) expires-in))
      (token_type . "Bearer")
      (client_id . ,(alist-get 'client_id old-data))
      (client_secret . ,(alist-get 'client_secret old-data)))))

;;;; OAuth flow

(defun gdocs-auth--start-oauth-flow (account &optional callback)
  "Start the OAuth 2.0 authorization flow for ACCOUNT.
Create a local HTTP server to receive the callback, then open the
user's browser to Google's consent screen.  CALLBACK, if
non-nil, is called with the access token string upon successful
authentication."
  (let* ((creds (gdocs-auth--account-credentials account))
         (client-id (alist-get 'client-id creds))
         (client-secret (alist-get 'client-secret creds))
         (server (gdocs-auth--start-callback-server
                  account client-id client-secret callback))
         (port (process-contact server :service))
         (redirect-uri (format "http://localhost:%d" port))
         (auth-url (gdocs-auth--build-auth-url client-id redirect-uri)))
    (message "Opening browser for Google OAuth consent...")
    (browse-url auth-url)))

(defun gdocs-auth--account-credentials (account)
  "Return the credentials alist for ACCOUNT from `gdocs-accounts'.
Signal an error if ACCOUNT is not found."
  (let ((entry (assoc account gdocs-accounts)))
    (unless entry
      (error "Account %s not found in `gdocs-accounts'" account))
    (cdr entry)))

(defun gdocs-auth--build-auth-url (client-id redirect-uri)
  "Build the Google OAuth authorization URL.
CLIENT-ID is the OAuth client ID.  REDIRECT-URI is the local
callback URL."
  (concat gdocs-auth--auth-url "?"
          (gdocs-auth--url-encode-params
           `(("client_id" . ,client-id)
             ("redirect_uri" . ,redirect-uri)
             ("response_type" . "code")
             ("scope" . ,gdocs-auth--scopes)
             ("access_type" . "offline")
             ("prompt" . "consent")))))

;;;; Callback server

(defun gdocs-auth--start-callback-server (account client-id
                                                  client-secret callback)
  "Start a local HTTP server to receive the OAuth callback.
ACCOUNT is the account name to store tokens for.  CLIENT-ID and
CLIENT-SECRET are the OAuth credentials used for the token
exchange.  CALLBACK, if non-nil, is called with the access token
on success.  Return the server process."
  (let ((server nil))
    (setq server
          (make-network-process
           :name "gdocs-oauth-callback"
           :server t
           :host "127.0.0.1"
           :service t
           :family 'ipv4
           ;; No accumulation needed: localhost OAuth redirects arrive in one segment
           :filter (lambda (proc data)
                     (gdocs-auth--handle-callback
                      proc data server
                      account client-id client-secret callback))
           :sentinel #'ignore))
    server))

(defun gdocs-auth--handle-callback (proc data server account
                                         client-id client-secret callback)
  "Handle an incoming HTTP request on the OAuth callback server.
PROC is the client connection process.  DATA is the raw HTTP
request string.  SERVER is the server process to clean up.
ACCOUNT, CLIENT-ID, and CLIENT-SECRET are used for the token
exchange.  CALLBACK, if non-nil, is called with the access token
on success."
  (let ((code (gdocs-auth--extract-auth-code data)))
    (if code
        (let ((redirect-uri (format "http://localhost:%d"
                                    (process-contact server :service))))
          (gdocs-auth--send-success-response proc)
          (delete-process server)
          (gdocs-auth--exchange-code-for-tokens
           code redirect-uri client-id client-secret account callback))
      ;; Non-auth request (favicon, prefetch, etc.): respond but keep
      ;; the server alive for the real OAuth redirect.
      (process-send-string
       proc "HTTP/1.1 404 Not Found\r\nContent-Length: 0\r\nConnection: close\r\n\r\n")
      (delete-process proc))))

(defun gdocs-auth--extract-auth-code (http-request)
  "Extract the authorization code from HTTP-REQUEST string.
Return the code string, or nil if not found."
  (when (string-match "[?&]code=\\([^& \r\n]+\\)" http-request)
    (match-string 1 http-request)))

(defun gdocs-auth--send-success-response (proc)
  "Send an HTTP success response to PROC and close the connection."
  (let ((body (concat "<html><body>"
                      "<h1>Authentication successful!</h1>"
                      "<p>You can close this tab and return to Emacs.</p>"
                      "</body></html>")))
    ;; Content-Length uses character count; safe here since body is ASCII-only
    (process-send-string
     proc
     (format (concat "HTTP/1.1 200 OK\r\n"
                     "Content-Type: text/html\r\n"
                     "Content-Length: %d\r\n"
                     "Connection: close\r\n\r\n%s")
             (length body) body))
    (delete-process proc)))

(defun gdocs-auth--send-error-response (proc)
  "Send an HTTP error response to PROC and close the connection."
  (let ((body (concat "<html><body>"
                      "<h1>Authentication failed</h1>"
                      "<p>Please try again.</p>"
                      "</body></html>")))
    (process-send-string
     proc
     (format (concat "HTTP/1.1 400 Bad Request\r\n"
                     "Content-Type: text/html\r\n"
                     "Content-Length: %d\r\n"
                     "Connection: close\r\n\r\n%s")
             (length body) body))
    (delete-process proc)))

;;;; Token exchange

(defun gdocs-auth--exchange-code-for-tokens (code redirect-uri client-id
                                                  client-secret account
                                                  callback)
  "Exchange authorization CODE for access and refresh tokens.
REDIRECT-URI is the callback URI used in the original request.
CLIENT-ID and CLIENT-SECRET are the OAuth credentials.  ACCOUNT
is the account name to store tokens under.  CALLBACK, if
non-nil, is called with the access token string on success."
  (plz 'post gdocs-auth--token-url
    :headers '(("Content-Type" . "application/x-www-form-urlencoded"))
    :body (gdocs-auth--url-encode-params
           `(("grant_type" . "authorization_code")
             ("code" . ,code)
             ("redirect_uri" . ,redirect-uri)
             ("client_id" . ,client-id)
             ("client_secret" . ,client-secret)))
    :as #'json-read
    :then (lambda (response)
            (let ((token-data (gdocs-auth--build-token-data
                               response client-id client-secret)))
              (gdocs-auth--write-token-file account token-data)
              (message "Successfully authenticated account %s" account)
              (when callback
                (funcall callback (alist-get 'access_token token-data)))))
    :else (lambda (err)
            (error "Failed to exchange authorization code for account %s: %s"
                   account (plz-error-message err)))))

(defun gdocs-auth--build-token-data (response client-id client-secret)
  "Build a token data alist from token endpoint RESPONSE.
CLIENT-ID and CLIENT-SECRET are stored alongside tokens so that
token refresh can proceed without re-reading `gdocs-accounts',
which may have changed or may not be loaded yet."
  (let ((access-token (alist-get 'access_token response))
        (refresh-token (alist-get 'refresh_token response))
        (expires-in (alist-get 'expires_in response)))
    `((access_token . ,access-token)
      (refresh_token . ,refresh-token)
      (expires_at . ,(+ (float-time) expires-in))
      (token_type . "Bearer")
      (client_id . ,client-id)
      (client_secret . ,client-secret))))

;;;; Utility functions

(defun gdocs-auth--url-encode-params (params)
  "URL-encode PARAMS alist into a query string.
PARAMS is an alist of (KEY . VALUE) string pairs.  Return a
string like \"key1=value1&key2=value2\"."
  (mapconcat (lambda (pair)
               (concat (url-hexify-string (car pair))
                       "="
                       (url-hexify-string (cdr pair))))
             params "&"))

(provide 'gdocs-auth)
;;; gdocs-auth.el ends here
