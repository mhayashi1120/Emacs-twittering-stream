;;; twittering-stream.el --- Twitter stream extension.

;; Author: Masahiro Hayashi <mhayashi1120@gmail.com>
;; Keywords: twitter user stream
;; Emacs: GNU Emacs 22 or later
;; Version: 0.0.6
;; Package-Requires: ((json "1.2") (twittering-mode "2.0"))

(defconst twittering-stream-version "0.0.6")
  
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; ## Usage:
;;
;;     M-x twittering-visit-timeline :stream/user

;;; TODO:

;; * mode-line `wget' string

;; * filter stream
;;   :stream/filter/some-id
;;   - each timeline has tracks and follows some of local var or alist of global
;;   - timer watch above config var and restart if need.
;;     (start proc -> stop current) `twittering-stream-resume'

;; * keep userstream wheather or not buffer is opening.

;;; Code:

(require 'twittering-mode)
(require 'json)

(defgroup twittering-stream ()
  "twittering-mode extensions using Twitter Streaming API."
  :prefix "twittering-"
  :group 'applications)

(defconst twittering-stream-userstream-url "https://userstream.twitter.com/1.1/user.json")
(defconst twittering-stream-buffer-basename " *Twittering Stream* ")

(defcustom twittering-stream-userstream-function
  'twittering-stream-wget-userstream
  "Create the twittering stream process function.
This function accept just one arg which indidate process buffer,
and return that process.
TODO only filter not add sentinel
TODO twittering-spec, cleanup-function property
"
  :group 'twittering-stream
  :type 'function)

(defcustom twittering-stream-show-error-p nil
  "Report error while retrieving JSON object from streaming API."
  :group 'twittering-stream
  :type 'boolean)

(defcustom twittering-stream-timeout 300
  "Number of seconds to timeout from streaming API.
After the timeout, reconnect to stream immediately."
  :group 'twittering-stream
  :type 'integer)

(defcustom twittering-stream-tls-checktrust 'ask
  "Pass to `tls-checktrust'"
  :group 'twittering-stream
  :type '(choice (const :tag "Always" t)
		 (const :tag "Never" nil)
		 (const :tag "Ask" ask)))

(defcustom twittering-stream-event-hook nil
  "Hook functions which call with two argument json object and twmode spec."
  :group 'twittering-stream
  :type 'hook)

;;;
;;; Extend twmode
;;;

(defun twittering-stream-extract-timeline-spec (str)
  (cond
   ((string-match "\\`:stream/user\\'" str)
    `((stream "user") . ""))))

(defun twittering-stream-retrieve-timeline (spec-string)
  (let* ((tlspec (twittering-stream-extract-timeline-spec spec-string))
         (spec (car-safe tlspec)))
    (cond
     ((null spec) nil)
     ((not (eq (car-safe spec) 'stream)) nil)
     ((twittering-stream--active-process spec) t)
     (t
      ;;TODO exclusive lock!
      (twittering-stream-connect spec)
      t))))

(defun twittering-stream-timeline-spec-primary-p (spec)
  (eq (car-safe spec) 'stream))

(defun twittering-stream--prepare-buffer-maybe (spec-or-string buffer)
  (let ((spec (if (stringp spec-or-string)
                  (car (twittering-stream-extract-timeline-spec spec-or-string))
                spec-or-string)))
    (cond
     ((not (eq (car-safe spec) 'stream)))
     (t
      (with-current-buffer buffer
        (let* ((args (cdr-safe spec))
               (type (car-safe args)))
          (cond
           ((equal type "user")
            ;; some of specialized code for userstream
            ;;TODO cleanup buffer when unload-feature
            ))))))))

(defun twittering-stream-timeline-spec-to-string (timeline-spec)
  (let ((type (car timeline-spec))
        (value (cdr timeline-spec)))
    (and (eq type 'stream)
         (concat ":stream/" (mapconcat 'identity value "/")))))

;;TODO not used
(defun twittering-stream-cleanup-buffer ()
  (let ((spec (twittering-current-timeline-spec)))
    (ignore-errors
      (twittering-stream-shutdown spec))))

(defun twittering-stream--get-buffer (spec)
  (loop for b in (twittering-get-buffer-list)
        if (with-current-buffer b
             (equal (twittering-current-timeline-spec) spec))
        return b))

(defadvice twittering-retrieve-timeline
  (around twittering-retrieve-timeline-ad (spec-string &rest _ignore)
          activate)
  (setq ad-return-value
        (or (twittering-stream-retrieve-timeline spec-string)
            ad-do-it)))

(defadvice twittering-get-managed-buffer
  (around twittering-get-managed-buffer-ad (spec) activate)
  (let ((buffer ad-do-it))
    (twittering-stream--prepare-buffer-maybe spec buffer)
    (setq ad-return-value buffer)))

(defadvice twittering-timeline-spec-to-string
  (around twittering-timeline-spec-to-string-ad
          (timeline-spec &optional shorten) activate)
  (setq ad-return-value 
        (or (twittering-stream-timeline-spec-to-string timeline-spec)
            ad-do-it)))

(defadvice twittering-extract-timeline-spec
  (around twittering-extract-timeline-spec-ad
          (str &optional unresolved-aliases) activate)
  (setq ad-return-value 
        (or (twittering-stream-extract-timeline-spec str)
            ad-do-it)))

(defadvice twittering-timeline-spec-primary-p
  (around twittering-timeline-spec-primary-p-ad (spec)
          activate)
  (setq ad-return-value
        (or (twittering-stream-timeline-spec-primary-p spec)
            ad-do-it)))

;;;
;;; Basic utilities
;;;

(defun twittering-stream--disconnect (spec)
  (let ((proc (twittering-stream--active-process spec)))
    (when proc
      (delete-process proc))))

(defun twittering-stream--buffer-live-p (spec)
  (twittering-stream--get-buffer spec))

(defun twittering-stream--all-active-processes ()
  (delq nil (mapcar
             (lambda (proc)
               (and (process-get proc 'twittering-spec)
                    (memq (process-status proc) '(run open connect))
                    proc))
             (process-list))))

(defun twittering-stream--active-process (spec)
  (let ((proc (catch 'found
                (dolist (proc (process-list))
                  (when (equal (process-get proc 'twittering-spec) spec)
                    (throw 'found proc))))))
    (when (and proc
               (memq (process-status proc) '(run open connect)))
      proc)))

(defun twittering-stream--oauth-token (method url params)
  (let ((access-token
         (cdr (assoc "oauth_token"
                     twittering-oauth-access-token-alist)))
        (access-token-secret
         (cdr (assoc "oauth_token_secret"
                     twittering-oauth-access-token-alist))))
    (twittering-oauth-auth-str-access 
     method url params
     twittering-oauth-consumer-key twittering-oauth-consumer-secret
     access-token access-token-secret)))

;;;
;;; http generic method
;;;

(defmacro twittering-stream--filter (proc event &rest form)
  (declare (indent 2))
  `(let ((buf (process-buffer ,proc)))
     (cond
      ((not (buffer-live-p buf)))
      (t
       (with-current-buffer buf
         (goto-char (point-max))
         (insert ,event)
         (goto-char (point-min))
         (progn ,@form))))))

(defun twittering-stream--retrieve-http-header (proc)
  (when (re-search-forward "^HTTP/\\(?:[0-9.]+\\)[ \t]+\\([0-9]+\\)" nil t)
    (let ((code (string-to-number (match-string 1))))
      (cond
       ((= code 200)
        (let ((inhibit-read-only t))
          (erase-buffer))
        ;; replace the filter
        (set-process-filter proc 'twittering-stream--json-filter))
       (t
        (message "Connect failed with HTTP Code: %s" code)
        (delete-process proc))))))

;;;
;;; Generic handler
;;;

(defun twittering-stream--json-filter (proc event)
  (let ((buf (process-buffer proc)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (save-excursion
          (let ((retry (process-get proc 'error-count))
                (spec (process-get proc 'twittering-spec))
                (inhibit-read-only t)
                json)
            (goto-char (point-max))
            (insert event)
            (goto-char (point-min))
            (condition-case err
                (while (setq json (twittering-stream--read-json))
                  ;; read succeeded even if one time, reset error count
                  (setq retry 0)
                  (delete-region (point-min) (point))
                  (process-put proc 'error-count 0)
                  (save-excursion
                    (ignore-errors
                      (run-hook-with-args 'twittering-stream-event-hook json spec)))
                  (save-excursion
                    (twittering-stream--twmode-handler json spec)))
              (error
               (cond
                ;; This filter receive event like following sequence
                ;; 1. {"a"
                ;; 2. :"
                ;; 3. A"}
                ;; step 1. raise error and step 2. either.
                ;; Arrived event string will be not so small but not too big.
                ;; Maximum retry == 5 satisfy the Twitter streaming API response.
                ((> retry 5)
                 (twittering-stream-show-error err event)
                 (erase-buffer))
                (t
                 (process-put proc 'error-count (1+ retry))))))))))))

(defun twittering-stream--read-json ()
  ;; skip if chunked size (hex)
  (skip-chars-forward "0-9a-fa-F\r\n\s")
  (unless (eobp)
    (json-read)))

(defun twittering-stream--sentinel (proc event)
  (when (memq (process-status proc) '(exit closed signal))
    (let ((buf (process-buffer proc)))
      (when (buffer-live-p buf)
        (kill-buffer buf)))
    (let ((spec (process-get proc 'twittering-spec))
          (cleanup (process-get proc 'cleanup-function)))
      (when (functionp cleanup)
        (funcall cleanup proc))
      (cond
       ((process-get proc 'suppress-reconnect))
       (t
        ;; retry twittering stream
        (run-with-timer
         1 nil
         'twittering-stream--try-reconnect spec 5))))))

(defun twittering-stream--try-reconnect (spec count)
  (with-local-quit
    (when (twittering-stream--buffer-live-p spec)
      ;;TODO continue user stream whether or not buffer is killed.
      (condition-case err
          (twittering-stream-connect spec)
        (error
         (message "Unable restart twittering stream: %s" err)
         (when (> count 0)
           ;; TODO expand wait time while failed to connect
           (run-with-timer
            10 nil 'twittering-stream--try-reconnect
            spec (1- count))))))))

(defun twittering-stream--twmode-handler (json spec)
  (case (twittering-stream--json-object-type json)
    (status
     (let ((statuses (list (twittering-json-object-to-a-status json))))
       (twittering-add-statuses-to-timeline-data statuses spec)))
    (mention
     ;;TODO
     (twittering-stream--mention json))))

(defvar twittering-stream-mention-arrived nil)
(defvar twittering-stream-twitter-icon
  (create-image
   "/* XPM */
static char *hoge[] = {
/* columns rows colors chars-per-pixel */
\"18 14 2 1\",
\"  c #00ACED\",
\". c None\",
/* pixels */
\"...........  .....\",
\". .......       ..\",
\".  ......       ..\",
\".    ....       ..\",
\"..             ...\",
\".              ...\",
\".              ...\",
\"..             ...\",
\"...           ....\",
\"...           ....\",
\"....         .....\",
\"....        ......\",
\".          .......\",
\"...     ..........\"
};"

   nil t :ascent 'center)
  "Generated by following command
convert twitter-bird-light-bgs.png -crop 190x150+58+75 -resize 18x18 hoge.xpm
")

(defun twittering-stream--mention (json)
  ;;TODO clear when open :mention tl?
  (setq twittering-stream-mention-arrived t)
  (twittering-stream--mode-line-update))

;;FIXME: how to exactly match to json object type
(defun twittering-stream--json-object-type (json)
  (cond
   ((and (assq 'created_at json)
         (assq 'text json))
    'status)
   ((assq 'delete json)
    'delete)
   ((assq 'mention json)
    'mention)
   ((assq 'friends json)
    'friends)
   ;;TODO
   (t 'unknown)))

(defun twittering-stream-show-error (err event)
  (when twittering-stream-show-error-p
    (message "%s %s" err event)
    (ding)))

(defvar twittering-stream-mode-string nil)
(put 'twittering-stream-mode-string 'risky-local-variable t)

(defun twittering-stream--mode-line-update ()
  (setq twittering-stream-mode-string
        ;;TODO quote
        (concat
         (and twittering-stream-mention-arrived
              (let ((icon (if (display-graphic-p)
                              twittering-stream-twitter-icon)))
                (concat " " (propertize "TW" 'display icon)))))))

(or (memq 'twittering-stream-mode-string global-mode-string)
    (setq global-mode-string
          (append global-mode-string '(twittering-stream-mode-string))))

;;;
;;; wget implementation
;;;

(defun twittering-stream-wget-userstream (buffer)
  (let* ((request
          `((uri . ,twittering-stream-userstream-url)
            (method . "POST")))
         (connection-info (twittering-make-connection-info request)))
    (twittering-stream-wget--open-userstream-filter buffer connection-info)))

(defun twittering-stream-wget--prepare-encode (post-form)
  (loop for (key . val) in post-form
        collect (cons
                 (twittering-percent-encode key)
                 (twittering-percent-encode val))))

(defun twittering-stream-wget--open-userstream-filter (buffer connection-info)
  (let* ((request (cdr (assq 'request connection-info)))
	 (method (cdr (assq 'method request)))
         (uri (cdr (assq 'uri request)))
         (uri-without-query (cdr (assq 'uri-without-query request)))
         (params (cdr (assq 'params request)))
         (use-proxy (cdr (assq 'use-proxy connection-info)))
         (proxy-server (cdr (assq 'proxy-server connection-info)))
         (proxy-port (cdr (assq 'proxy-port connection-info)))
         (proxy-user (cdr (assq 'proxy-user connection-info)))
         (proxy-password (cdr (assq 'proxy-password connection-info)))
         (proxy-credentials
          (when (and proxy-user proxy-password)
            (concat "Basic "
                    (base64-encode-string
                     (concat proxy-user ":" proxy-password)))))
         (allow-insecure-server-cert
          (cdr (assq 'allow-insecure-server-cert connection-info)))
         (cacert-fullpath (cdr (assq 'cacert-fullpath connection-info)))
         (cacert-filename (when cacert-fullpath
                            (file-name-nondirectory cacert-fullpath)))
         (args
          `("--save-headers"
            "--quiet"
            ,(format "--output-file=%s" null-device)
            "--output-document=-"
            ,(format "--read-timeout=%d" twittering-stream-timeout)
            ,@(when cacert-filename
                `(,(format "--ca-certificate=%s" cacert-filename)))
            ,@(when allow-insecure-server-cert
                `("--no-check-certificate"))
            ,@(unless use-proxy
                '("--no-proxy"))
            ,@(when (equal method "POST")
                `("--post-data" ""))
            ,uri))
         (hidden-args
          `(
            ,@(when (and use-proxy proxy-server proxy-port
                         proxy-user proxy-password)
                `(,(cons "proxy_user" proxy-user)
                  ,(cons "proxy_password" proxy-password)))
            ("header" .
             ,(format "%s: %s"
                      "Authorization"
                      (twittering-stream--oauth-token
                       method (or uri-without-query uri) params)))))
         (coding-system-for-read 'binary)
         (coding-system-for-write 'binary)
         (command (twittering-find-wget-program)))
    (let ((process-environment (copy-sequence process-environment))
          (wgetrc (twittering-stream-wget--create-rcfile hidden-args)))
      (setenv "WGETRC" wgetrc)
      (let ((proc (apply 'start-process "Twittering stream" buffer command args)))
        (set-process-filter proc 'twittering-stream-wget--header-filter)
        (process-put proc 'cleanup-function 'twittering-stream-wget--cleanup)
        (process-put proc 'wgetrc-file wgetrc)
        proc))))

(defun twittering-stream-wget--cleanup (proc)
  (let ((wgetrc (process-get proc 'wgetrc-file)))
    (when (and (stringp wgetrc) (file-exists-p wgetrc))
      (delete-file wgetrc))))

(defun twittering-stream-wget--create-rcfile (alist)
  (let* ((prefix (expand-file-name "twmode-wgetrc" temporary-file-directory))
         (temp (make-temp-name prefix)))
    (with-temp-buffer
      (dolist (pair alist)
        (insert (format "%s = %s\n" (car pair) (cdr pair))))
      (let ((coding-system-for-write 'raw-text))
        (write-region (point-min) (point-max) temp nil 'no-msg)))
    (set-file-modes temp ?\400)
    temp))

;; Handle http header and switch filter itself to json filter.
(defun twittering-stream-wget--header-filter (proc event)
  (twittering-stream--filter proc event
    (twittering-stream--retrieve-http-header proc)))

;;;
;;; Emacs network interface
;;;

(defun twittering-stream-network-userstream (buffer)
  (twittering-stream-network--userstream-1
   buffer "POST" twittering-stream-userstream-url))

(defun twittering-stream-network--header-filter (proc event)
  (twittering-stream--filter proc event
    (twittering-stream--retrieve-http-header proc)))

(defun twittering-stream-network--userstream-1 (buffer method uri)
  (require 'tls)
  (let* ((urlobj (url-generic-parse-url uri))
         (host (url-host urlobj))
         (port (url-port urlobj))
         (path (url-filename urlobj))
         (user-agent (format "Twittering-Stream (Emacs) / %s" twittering-stream-version))
         ;;TODO reconsider it
         (size 0)
         (params nil)
         (oauth (twittering-stream--oauth-token method uri params))
         ;; dynamic bind
         (tls-checktrust twittering-stream-tls-checktrust))
    (let ((proc (open-network-stream
                 "Twittering stream" buffer host port
                 :type 'ssl)))

      (unless proc
        (error "Process is not running"))

      (process-send-string proc (format "%s %s HTTP/1.1\r\n" method path))
      (process-send-string proc (format "Content-Length: %s\r\n" size))
      (process-send-string proc (format "User-Agent: %s\r\n" user-agent))
      (process-send-string proc (format "Host: %s\r\n" host))
      (process-send-string proc (format "Authorization: %s\r\n" oauth))
      (process-send-string proc "\r\n")

      (set-process-filter proc 'twittering-stream-network--header-filter)
      proc)))

;;;
;;; Commands
;;;

(defun twittering-stream--shutdown-process (proc)
  (let ((spec (process-get proc 'twittering-spec)))
    (twittering-stream--disconnect spec)
    (message "Disconnected %s at %s"
             spec (current-time-string))))

(defun twittering-stream-restart ()
  "Force restart stream."
  (interactive)
  (let ((spec (twittering-current-timeline-spec)))
    (unless (eq (car-safe spec) 'stream)
      (error "Not a streaming timeline"))
    (twittering-stream-shutdown spec)
    (twittering-stream-connect spec)))

(defun twittering-stream-resume (spec)
  "Continue to connect stream."
  (interactive
   (list (twittering-current-timeline-spec)))
  (unless (eq (car-safe spec) 'stream)
    (error "Not a streaming timeline"))
  ;; keep current session if exists, and start another process
  ;; to suppress drop a packet.
  (let ((active (twittering-stream--active-process spec))
        (newbie (twittering-stream-connect spec)))
    (when active
      (twittering-stream--shutdown-process active))
    (message "Resuming %s at %s"
             spec (current-time-string))
    newbie))

(defun twittering-stream-shutdown (spec)
  "Shutdown the active stream if exists."
  (interactive
   (list (twittering-current-timeline-spec)))
  (unless spec
    (error "Not a streaming timeline"))
  (let ((proc (twittering-stream--active-process spec)))
    (twittering-stream--shutdown-process proc)))

(defun twittering-stream-shutdown-all ()
  "Shutdown all twitter streaming processes."
  (interactive)
  (let ((procs (twittering-stream--all-active-processes)))
    (dolist (proc procs)
      (twittering-stream--shutdown-process proc))))

(defun twittering-stream-connect (spec)
  "Connect twitter streaming API"
  (unless (eq twittering-account-authorization 'authorized)
    (error "Twittering-mode is not authorized"))
  (let* ((type (cadr spec))
         (args (cddr spec))         ; not used now.
         ;;TODO if remaining zombie process
         (buf (generate-new-buffer twittering-stream-buffer-basename))
         (proc (cond
                ((equal type "user")
                 (funcall twittering-stream-userstream-function buf))
                (t (error "Not supported type %s" type)))))
    (set-process-sentinel proc 'twittering-stream--sentinel)
    (process-put proc 'error-count 0)
    (process-put proc 'twittering-spec spec)
    (message "Connected to twitter stream at %s (%s)"
             (current-time-string)
             (twittering-timeline-spec-to-string spec))
    proc))

;;;
;;; Unloader
;;;

(defun twittering-stream-unload-function ()
  (twittering-stream-shutdown-all)
  (setq global-mode-string
        (delq 'twittering-stream-mode-string global-mode-string))
  ;;TODO check really unadviced
  (ad-remove-advice 'twittering-timeline-spec-primary-p
                    'around 'twittering-timeline-spec-primary-p-ad)
  (ad-remove-advice 'twittering-get-managed-buffer
                    'around 'twittering-get-managed-buffer-ad)
  (ad-remove-advice 'twittering-extract-timeline-spec
                    'around 'twittering-extract-timeline-spec-ad)
  (ad-remove-advice 'twittering-timeline-spec-to-string
                    'around 'twittering-timeline-spec-to-string-ad))

(provide 'twittering-stream)

;;; twittering-stream.el ends here
