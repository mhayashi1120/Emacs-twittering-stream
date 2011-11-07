;;TODO
;; * show unseen count or unseen existence.
;; * show icon in tab if private TL.
;; * manipulate by mouse.

(require 'twittering-mode)

(defvar twittering+tab-width 20)

(defface twittering+tab-unselected
  '((((type x w32 mac ns) (class color))
     :background "Gray70" :foreground "Gray20"
     :box (:line-width -1 :style released-button))
    (((class color))
     (:background "blue" :foreground "black")))
  "*Face to fontify unselected tabs."
  :group 'faces)

(defface twittering+tab-selected
  '((((type x w32 mac ns) (class color))
     :background "Gray90" :foreground "black"
     :box (:line-width -1 :style released-button))
    (((class color))
     (:background "cyan" :foreground "black"))
    (t (:underline t)))
  "*Face to fontify selected tab."
  :group 'faces)

(defface twittering+tab-background
  '((((type x w32 mac ns) (class color))
     :background "LightSteelBlue" :foreground "black")
    (((class color))
     (:background "white" :foreground "black")))
  "*Face to fontify background of tab line."
  :group 'faces)

(defface twittering+tab-selected-background
  '((((type x w32 mac ns) (class color))
     :background "LightSteelBlue" :foreground "black")
    (((class color))
     (:background "white" :foreground "black")))
  "*Face to fontify selected background tab."
  :group 'faces)

(defface twittering+tab-mouse
  '((((type x w32 mac ns) (class color))
     :background "Gray75" :foreground "white"
     :box (:line-width -1 :style released-button)))
  "*Face used to highlight tabs under the mouse."
  :group 'faces)

(defvar twittering+tab-map (make-sparse-keymap))

(when twittering+tab-map
  (let ((map twittering+tab-map))
    ;;TODO
    ))

(defvar twittering+tab-mode nil)
(defun twittering+tab-mode (&optional arg)
  "Toggle twittering tab emulation."
  (interactive "P")
  (setq twittering+tab-mode
        (if (null arg)
            (not twittering+tab-mode)
          (< 0 (prefix-numeric-value arg))))
  (cond
   (twittering+tab-mode
    (mapc
     (lambda (buf)
       (with-current-buffer buf
         (twittering+tab-initialize)))
     (twittering-get-buffer-list))
    (add-hook 'twittering-mode-hook 'twittering+tab-initialize))
   (t
    (mapc
     (lambda (buf)
       (with-current-buffer buf
         (kill-local-variable 'header-line-format)))
     (twittering-get-buffer-list))
    (remove-hook 'twittering-mode-hook 'twittering+tab-initialize))))

(defun twittering+tab-initialize ()
  (setq header-line-format 
        '(:eval (twittering+tab-line))))

(defun twittering+tab-background-propertize (string)
  (let ((end (length string)))
    (add-text-properties 0 end 
                         (list
                          'face (list 'twittering+tab-background)
                          'mouse-face 'twittering+tab-selected-background
                          'tab-separator t)
                         string)
    string))

(defvar twittering+tab-separator
  (let ((sep " "))
    (twittering+tab-background-propertize sep)
    (propertize sep 'display 
                '(space :width 0.2)))
  "String used to separate tabs.")

;; copied from w3m-ems.el
(defvar twittering+tab-timer nil
  "Internal variable used to say time has not gone by after the tab-line
was updated last time.  It is used to control the `twittering+tab-line'
function running too frequently, set by the function itself and
cleared by a timer.")
(make-variable-buffer-local 'twittering+tab-timer)

(defvar twittering+tab-line-format nil)
(make-variable-buffer-local 'twittering+tab-line-format)

(defun twittering+tab-line ()
  "twittering-mode tab extension."
  (or (and twittering+tab-timer twittering+tab-line-format)
      (let* ((bufs (twittering-get-buffer-list))
             (fringes (window-fringes))
             (fringe-width (truncate
                            (/ (float (+ (or (nth 0 fringes) 0)
                                         (or (nth 1 fringes) 0)))
                               ;; pixel of char
                               (frame-char-width))))
             (win-width (+ (window-width)
                           fringe-width
                           ;; Assume that the vertical scroll-bar has
                           ;; the width of two space characters.
                           (if (car (frame-current-scroll-bars)) 2 0)))
             (breadth twittering+tab-width)
             (current (current-buffer))
             (maxtabs (- (/ win-width breadth)
                         (if (< (% win-width breadth) (/ breadth 2)) 1 0)))
             (groups (twittering+tab-groups current bufs maxtabs))
             (main-format (mapconcat 
                           (lambda (buf)
                             (let ((name (twittering+tab-name buf breadth)))
                               (propertize name 'face 
                                           (if (eq buf current)
                                               (list 'twittering+tab-selected)
                                             (list 'twittering+tab-unselected)))))
                           (nth 1 groups)
                           twittering+tab-separator))
             (main-width (string-width main-format))
             (rest-width (- win-width main-width)))
        ;; suppress flickering tab
        (setq twittering+tab-timer t)
        (run-at-time 0.1 nil
                     (lambda (buffer)
                       (when (buffer-live-p buffer)
                         (with-current-buffer buffer
                           (setq twittering+tab-timer nil))))
                     current)
        (setq twittering+tab-line-format
              (concat
               (and (nth 0 groups)
                    (concat
                     (let ((name (twittering+tab-name (nth 0 groups)
                                                      (if (nth 2 groups)
                                                          (/ rest-width 2)
                                                        rest-width))))
                       (propertize name 'face (list 'twittering+tab-unselected)))
                     twittering+tab-separator))
               main-format
               (and (nth 2 groups)
                    (concat
                     twittering+tab-separator
                     (let ((name (twittering+tab-name (nth 2 groups)
                                                      (if (nth 0 groups)
                                                          (/ rest-width 2)
                                                        rest-width))))
                       (propertize name 'face (list 'twittering+tab-unselected)))))
               ;; fill background to full width
               (twittering+tab-background-propertize
                (propertize 
                 (make-string win-width ?\ ))))))))

(defun twittering+tab-name (buffer breadth)
  (let* ((name (buffer-name buffer)))
    (propertize
     (cond
      ((or (< breadth 6)
           (<= (string-width name) breadth))
       (truncate-string-to-width name breadth nil ?\ ))
      (t
       (concat (truncate-string-to-width name (- breadth 3) nil ?\ ) "...")))
     'mouse-face (list 'twittering+tab-mouse)
     'local-map twittering+tab-map
     'help-echo name)))

(defun twittering+tab-groups (current bufs maxtabs)
  "(first-tab (normal-tabs) last-tab)"
  (let ((index (position current bufs))
        (nbufs (length bufs)))
    ;; `xxxx' means current buffer
    (cond
     ((<= nbufs maxtabs)
      ;; xxxx|----
      ;; ----|xxxx
      (list nil bufs nil))
     ((= (1+ index) nbufs)
      ;; ---|----|xxxx
      (let ((n (- nbufs maxtabs 1)))
        (list (nth n bufs) (cdr (nthcdr n bufs)) nil)))
     ((<= maxtabs index)
      ;; -|----|xxxx|-
      (let ((rev (memq current (reverse bufs))))
        (list 
         (car (last rev))
         (reverse (twittering-take rev maxtabs))
         (car (cdr (memq current bufs))))))
     (t
      ;; xxxx|----|---
      ;; ----|xxxx|---
      (let ((rest (twittering-take bufs (1+ maxtabs))))
        (list nil
              (twittering-take rest maxtabs) 
              (car (last rest))))))))

(defun twittering-take (list count)
  (let ((i 0)
        (res '()))
    (while (and list (< i count))
      (setq res (cons (car list) res))
      (setq list (cdr list))
      (incf i))
    (nreverse res)))

(provide 'twittering+tab)