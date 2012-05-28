;;; twittering-stream.el --- Twitter stream extension.

;; Author: Masahiro Hayashi <mhayashi1120@gmail.com>
;; Keywords: twitter user stream
;; Emacs: GNU Emacs 22 or later
;; Version: 0.0.2
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
;; parameter

;;; Usage:
;;
;; M-x twittering-stream-mode 

(require 'twittering-mode)
(require 'json)

(defconst twittering-stream-user-url "https://userstream.twitter.com/2/user.json")
(defconst twittering-stream-buffer-name " *Twittering Stream* ")

(defvar twittering-stream-mode nil)
(defun twittering-stream-mode (&optional arg)
  (interactive "P")
  "Streaming API for Twitter"
  (let ((proc (get-buffer-process twittering-stream-buffer-name)))
    (cond
     ((or (and (numberp arg) (minusp arg))
          (and (null arg)
               twittering-stream-mode))
      (when proc
        (delete-process proc))
      (setq twittering-stream-mode nil)
      (message "Twittering stream disabled."))
     (t
      (unless (eq twittering-account-authorization 'authorized)
        (twittering-stream-mode -1)
        (error "Twittering-mode is not authorized"))
      (unless proc
        (twittering-stream--open 
         twittering-stream-user-url))
      (setq twittering-stream-mode t)
      (message "Twittering stream enabled.")))))

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

;;TODO &optional arg
(defun twittering-stream--open (uri)
  (let* ((connection-info (twittering-make-connection-info '()))
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
         (cacert-dir (when cacert-fullpath
                       (file-name-directory cacert-fullpath)))
         (cacert-filename (when cacert-fullpath
                            (file-name-nondirectory cacert-fullpath)))
         (args
          `("--save-headers"
            "--quiet"
            ,(format "--output-file=%s" null-device)
            "--output-document=-"
            ,@(remove nil
                      (mapcar
                       (lambda (pair)
                         (unless (string= (car pair) "Host")
                           (format "--header=%s: %s" (car pair) (cdr pair))))
                       (list
                        (cons "Authorization" (twittering-stream--oauth-token uri)))))
            ,@(when cacert-filename
                `(,(format "--ca-certificate=%s" cacert-filename)))
            ,@(when allow-insecure-server-cert
                `("--no-check-certificate"))
            ,@(cond
               ((not use-proxy)
                '("--no-proxy"))
               ((and use-proxy proxy-server proxy-port
                     proxy-user proxy-password)
                `(,(format "--proxy-user=%s" proxy-user)
                  ,(format "--proxy-password=%s" proxy-password)))
               (t
                nil))
            ,uri))
         (coding-system-for-read 'binary)
         (coding-system-for-write 'binary)
         (buffer (get-buffer-create twittering-stream-buffer-name))
         (command (twittering-find-wget-program))
         (proc (apply 'start-process "Twittering stream" buffer command args))
         )
    (set-process-filter proc 'twittering-stream--wget-header-filter)))

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
              ;; replace the filter
              (set-process-filter proc 'twittering-stream--wget-filter)
              (process-put proc 'twittering-stream--error-count 0))
             (t
              (message "Stream process exited abnormally with HTTP Code: %s" code)))))))))

(defvar twittering-stream-handler-function 
  'twittering-stream--default-handler)

(defun twittering-stream--wget-filter (proc event)
  (let ((buf (process-buffer proc))
        (cbuf (current-buffer))
        (mode major-mode))              ; current mode
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
                (while (setq json (json-read))
                  (delete-region (point-min) (point))
                  (process-put proc 'twittering-stream--error-count 0)
                  (when (or (not twittering-stream-suppress-message)
                            (eq mode 'twittering-mode))
                    (with-current-buffer cbuf
                      (save-excursion
                        (when (funcall twittering-stream-handler-function json)
                          (ding))))))
              ;; ignore eob
              (end-of-file)
              (error
               (cond
                ((> retry 5)
                 (erase-buffer))
                (t
                 (process-put proc
                              'twittering-stream--error-count
                              (1+ retry))))))))))))

(defconst twittering-stream--replace-table
  '(
    ("&gt;" . ">")
    ("&lt;" . "<")
    ("&quot;" . "\"")
    ("&amp;" . "&")
    ))

(defun twittering-stream--html2text (text)
  (loop with tmp = text
        for pair in twittering-stream--replace-table
        do (setq tmp (replace-regexp-in-string (car pair) (cdr pair) tmp))
        finally return tmp))

;;TODO
(defvar twittering-stream-suppress-message t)

(defun twittering-stream--default-handler (json)
  (or
   (twittering-stream--default-tweet-handler json)
   (twittering-stream--default-event-handler json)))

(defun twittering-stream--default-event-handler (json)
  (let* ((event (cdr (assq 'event json)))
         (source (cdr (assq 'source json)))
         (name (cdr (assq 'screen_name source))))
    (and event name
         (twittering-stream--message "[%s] Event: %s" name event))))

(defun twittering-stream--default-tweet-handler (json)
  (let* ((user (cdr (assq 'user json)))
         (name (cdr (assq 'screen_name user)))
         (text (cdr (assq 'text json))))
    (and name text
         (twittering-stream--message "[%s] %s" name text))))

(defun twittering-stream--message (fmt &rest args)
  (let* ((msg (apply 'format fmt args))
         (truncated (twittering-stream-truncate msg))
         message-log-max)
    (message "%s" (twittering-stream--html2text truncated))
    t))

(defvar twittering-stream--popup-buffer " *Twittering Stream Popup* ")
(defvar twittering-stream--popup-window-config nil)

(defun twitteirng-stream--popup (buffer)
  (let* ((alist (mapcar (lambda (w) (cons (window-buffer w) w)) (window-list)))
         (win (or (cdr (assq buffer alist))
                  (progn
                    (setq twittering-stream--popup-window-config
                          (current-window-configuration))
                    (let ((w (split-window nil (- (window-height) 4))))
                      (set-window-buffer w buffer)
                      w)))))
    (cancel-function-timers 'twittering-stream-popup-cleanup)
    (let ((old-win (selected-window)))
      (select-window win)
      (goto-char (point-max))
      (set-window-start win (line-beginning-position -1))
      (select-window old-win))
    (run-at-time 3 nil 'twittering-stream-popup-cleanup)))

(defun twittering-stream-popup-cleanup ()
  (when (window-configuration-p twittering-stream--popup-window-config)
    (set-window-configuration twittering-stream--popup-window-config)
    (setq twittering-stream--popup-window-config nil)))

(defun twittering-stream-popup-handler (json)
  (let* ((notify (twittering-stream-popup-single-line json))
         (buffer (get-buffer-create twittering-stream--popup-buffer)))
    (when (stringp notify)
      (with-current-buffer buffer
        (when (> (line-number-at-pos (point-max)) 
                 (* twittering-stream-popup-max-threshold 2))
          (goto-char (point-max))
          (forward-line (- twittering-stream-popup-max-threshold))
          (delete-region (point-min) (point)))
        (goto-char (point-max))
        (insert notify "\n"))
      (twitteirng-stream--popup buffer)
      t)))

(defvar twittering-stream-popup-max-threshold 300)

(defun twittering-stream-popup-single-line (json)
  (or
   (let* ((user (cdr (assq 'user json)))
          (name (cdr (assq 'screen_name user)))
          (text (cdr (assq 'text json))))
     (and name text
          (string-match (format "@%s\\b" twittering-username) text)
          (twittering-stream-truncate (format "[%s] %s" name text))))
   (let* ((event (cdr (assq 'event json)))
          (source (cdr (assq 'source json)))
          (name (cdr (assq 'screen_name (and (consp source) source)))))
     (and event name
          (format "[%s] Event: %s" name event)))))

(defun twittering-stream-truncate (msg)
  (truncate-string-to-width
   msg (max 0 (- (frame-width) 10)) nil nil "..."))

(provide 'twittering-stream)

;;; twittering-stream.el ends here
