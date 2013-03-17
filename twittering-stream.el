;;; twittering-stream.el --- Twitter stream extension.

;; Author: Masahiro Hayashi <mhayashi1120@gmail.com>
;; Keywords: twitter user stream
;; Emacs: GNU Emacs 22 or later
;; Version: 0.0.3
;; Package-Requires: ((json "1.2") (twittering-mode "2.0"))

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

;;; TODO:

;; check wget implementation is work.

;;; Usage:
;;
;; M-x twittering-visit-timeline :stream/user

(require 'twittering-mode)
(require 'json)

(defgroup twittering-stream ()
  "twittering-mode extensions using Twitter Streaming API."
  :group 'applications)

(defconst twittering-stream-version "0.0.3")
  
(defconst twittering-stream-user-url "https://userstream.twitter.com/1.1/user.json")
(defconst twittering-stream-buffer-name " *Twittering Stream* ")

(defcustom twittering-stream-open-function 'twittering-stream--open-wget-filter
  "Open twittering stream process function.
This function must execute any process that work under `twittering-stream-buffer-name' buffer.
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

(defun twittering-stream-restart ()
  "Force restart stream."
  (interactive)
  (twittering-stream-shutdown)
  (twittering-stream-connect))

(defun twittering-stream-shutdown ()
  "Shutdown the active stream if exists."
  (interactive)
  (let ((proc (twittering-stream-active-process)))
    (when proc
      (process-put proc 'twittering-stream-suppress-reconnect t)
      (twittering-stream-disconnect))))

(defun twittering-stream-disconnect ()
  (let ((proc (twittering-stream-active-process)))
    (when proc
      (delete-process proc))))

(defun twittering-stream-active-process ()
  (let ((proc (get-buffer-process twittering-stream-buffer-name)))
    (when (and proc
               (memq (process-status proc) '(run open connect)))
      proc)))

(defun twittering-stream-connect ()
  "Connect twitter streaming API"
  (unless (eq twittering-account-authorization 'authorized)
    (error "Twittering-mode is not authorized"))
  (let ((buf (get-buffer-create twittering-stream-buffer-name)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (kill-all-local-variables)
        (erase-buffer)))
    (funcall twittering-stream-open-function buf)
    (message "Connected to twitter stream. (%s)"
             (current-time-string))))

(defun twittering-stream--oauth-token (url)
  (let ((access-token
         (cdr (assoc "oauth_token"
                     twittering-oauth-access-token-alist)))
        (access-token-secret
         (cdr (assoc "oauth_token_secret"
                     twittering-oauth-access-token-alist))))
    (twittering-oauth-auth-str-access 
     "GET" url '()
     twittering-oauth-consumer-key twittering-oauth-consumer-secret
     access-token access-token-secret)))

;;;
;;; Extend twmode
;;;

(defadvice twittering-retrieve-timeline
  (around twittering-retrieve-timeline-ad (spec-string &rest _ignore)
          activate)
  (setq ad-return-value
        (or (twittering-stream-retrieve-timeline spec-string)
            ad-do-it)))

(defadvice twittering-get-managed-buffer
  (around twittering-get-managed-buffer-ad (spec) activate)
  (let ((buffer ad-do-it))
    (twittering-stream-prepare-buffer-maybe spec buffer)
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

(defun twittering-stream-retrieve-timeline (spec-string)
  (let ((spec (twittering-stream-extract-timeline-spec spec-string)))
    (when spec
      (unless (twittering-stream-active-process)
        (twittering-stream-connect))
      t)))

(defun twittering-stream-timeline-spec-primary-p (spec)
  (eq (car-safe spec) 'stream))

(defun twittering-stream-prepare-buffer-maybe (spec buffer)
  (when (eq (car-safe spec) 'stream)
    ;; do nothing currently..
    ;; todo kill buffer hook and disconnect from stream?
    ))

(defun twittering-stream-timeline-spec-to-string (timeline-spec)
  (let ((type (car timeline-spec))
             (value (cdr timeline-spec)))
    (and (eq type 'stream)
         (concat ":stream/" (mapconcat 'identity value "")))))

(defun twittering-stream-extract-timeline-spec (str)
  (and (string-match "^:stream/user" str)
       `((stream "user") . "")))

;;;
;;; wget implementation
;;;

(defun twittering-stream--open-wget-filter (buffer)
  (let* ((uri twittering-stream-user-url)
         (connection-info (twittering-make-connection-info '()))
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
            ,uri))
         (hidden-args
          `(
            ,@(when (and use-proxy proxy-server proxy-port
                         proxy-user proxy-password)
                `(,(cons "proxy_user" proxy-user)
                  ,(cons "proxy_password" proxy-password)))
            ,(cons "header"
                   (format "%s: %s"
                           "Authorization"
                           (twittering-stream--oauth-token uri)))))
         (coding-system-for-read 'binary)
         (coding-system-for-write 'binary)
         (command (twittering-find-wget-program)))
    (let ((process-environment (copy-sequence process-environment))
          (wgetrc (twittering-stream--create-wgetrc hidden-args)))
      (setenv "WGETRC" wgetrc)
      (let ((proc (apply 'start-process "Twittering stream" buffer command args)))
        (set-process-filter proc 'twittering-stream--wget-header-filter)
        (set-process-sentinel proc 'twittering-stream--sentinel)))))

(defun twittering-stream--create-wgetrc (alist)
  ;;TODO do not consider about coding-system
  (let* ((prefix (expand-file-name "twmode-wgetrc" temporary-file-directory))
         (temp (make-temp-name prefix)))
    (with-temp-buffer
      (dolist (pair alist)
        (insert (format "%s = %s\n" (car pair) (cdr pair))))
      (write-region (point-min) (point-max) temp nil 'no-msg))
    (set-file-modes temp ?\400)
    temp))

;; Handle http header and switch filter itself to json filter.
(defun twittering-stream--wget-header-filter (proc event)
  (let ((buf (process-buffer proc)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (string-match "^HTTP/\\(?:[0-9.]+\\)[ \t]+\\([0-9]+\\)" event)
          (let ((code (string-to-number (match-string 1 event))))
            (cond
             ((= code 200)
              (let ((inhibit-read-only t))
                (erase-buffer))
              (process-put proc 'twittering-stream--error-count 0)
              ;; replace the filter
              (set-process-filter proc 'twittering-stream--json-filter))
             (t
              (message "Stream process exited abnormally with HTTP Code: %s" code)))))))))

(defun twittering-stream--json-filter (proc event)
  (let ((buf (process-buffer proc)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (save-excursion
          (let ((retry (process-get proc 'twittering-stream--error-count))
                (inhibit-read-only t)
                json)
            (goto-char (point-max))
            (insert-before-markers event)
            (goto-char (point-min))
            (condition-case err
                (while (setq json (twittering-stream-read-json))
                  (delete-region (point-min) (point))
                  (process-put proc 'twittering-stream--error-count 0)
                  (save-excursion
                    (twittering-stream--twmode-handler json)))
              (error
               (twittering-stream-show-error err event)
               (cond
                ((> retry 5)
                 (erase-buffer))
                (t
                 (process-put proc
                              'twittering-stream--error-count
                              (1+ retry))))))))))))

(defun twittering-stream-show-error (err event)
  (when twittering-stream-show-error-p
    (message "%s %s" err event)
    (ding)))

(defun twittering-stream-read-json ()
  (condition-case err
      (json-read)
    ;; Read from end of buffer
    (end-of-file nil)
    ;; {"a":1
    (wrong-type-argument nil)
    ;; "\u6cc
    (json-string-escape nil)
    ;; DO NOT catch unexpected error.
    ))

(defun twittering-stream--sentinel (proc event)
  (cond
   ((null (twittering-stream-active-process)))
   ((null (process-get proc 'twittering-stream-suppress-reconnect)))
   (t
    ;; retry twittering stream
    (twittering-stream--reconnect 5))))

(defun twittering-stream--reconnect (count)
  (condition-case err
      (twittering-stream-connect)
    (error
     (message "Unable restart twittering stream: %s" err)
     (when (> count 0)
       (run-with-timer 1 nil 'twittering-stream--reconnect (1- count))))))

(defun twittering-stream--twmode-handler (json)
  (case (twittering-stream--json-object-type json)
    (status
     (let* ((statuses (list (twittering-json-object-to-a-status json)))
            (spec '(stream "user")))
       (twittering-add-statuses-to-timeline-data statuses spec)))))

;;FIXME: how to exactly match to json object type
(defun twittering-stream--json-object-type (json)
  (cond
   ((and (assoc 'created_at json)
         (assoc 'text json))
    'status)
   ((assoc 'delete json)
    'delete)
   ;;TODO
   (t 'unknown)))

(defun twittering-stream-unload-function ()
  (twittering-stream-shutdown)
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
