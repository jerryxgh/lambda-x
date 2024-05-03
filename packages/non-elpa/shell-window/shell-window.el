;;; shell-window.el --- A minor mode shows shell like buffers -*- lexical-binding:t -*-

;; Copyright (C) 2023 GuanghuiXu

;; Author: GuanghuiXu gh_xu@qq.com
;; Maintainer: GuanghuiXu gh_xu@qq.com
;; Created: 2015-4-28
;; URL: https://github.com/jerryxgh/shell-window
;; Keywords: extensions, convenience
;; Version: 0.0.1
;; Package-Version: 0.0.1
;; Package-Requires: ((emacs "24.4"))
;;

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Shell window is a window for shell buffers, temp buffers and etc.

;; This minor mode let shell like buffers share a window, called shell window,
;; the shell window is always at the bottom of Emacs window.  Besides that, when
;; point move in or out the shell window, it will be enlarged or shrinked
;; automaticly.
;;
;; To use shell-window, place this file to load path and add this to .emacs:
;;
;; (require 'shell-window)
;; (shell-window-mode 1)
;;
;; or run M-x shell-window-mode to toggle it.
;;
;; To switch between buffers of shell window, you can bind keys like:
;;     (define-key shell-window-mode-map (kbd "C-c s") 'shell-window-switch-buffer)
;;
;; Then try run M-x shell or or eshell, if you want to show more buffers in
;; shell window, please customize variable: shell-window-buffers.

;;; Change Log:

;;; Code:

(require 'ido)
(require 'esh-mode)
(require 'comint)

(defgroup shell-window nil
  "A minor mode show shell like buffers in window anchored to the bottom of frame."
  :prefix "shell-window-"
  :version "0.0.1"
  :group 'convenience
  :group 'windows)

(defcustom shell-window-buffers
  '(;; Emacs scratch buffer
    "*scratch*"
    ("^\\*.*?vterm\\*\\(<.*>\\)?$" :regexp t)
    ;; shell and eshell buffers
    ("^\\*.*?shell\\*\\(<.*>\\)?$" :regexp t)
    ("^\\*shell.*\\*\\(<.*>\\)?$" :regexp t)
    "*terminal*"
    "*ansi-term*")
  "Configuration of buffers to be shown in shell window."
  :type '(repeat
          (cons :tag "Config"
                (choice :tag "Pattern"
                        (string :tag "Buffer Name")
                        (symbol :tag "Major Mode"))
                (plist :tag "Keywords"
                       :value (:regexp nil)
                       :options
                       ((:regexp (boolean :tag "On/Off"))))))
  :get (lambda (symbol)
         (mapcar (lambda (element)
                   (if (consp element)
                       element
                     (list element)))
                 (default-value symbol)))
  :group 'shell-window)

(defvar shell-window-auto-resize t
  "Whether resize shell window automatically.
If t, when switch out `shell-window-buffers', shrink shell window
to `shell-window-min-window-height', when switch in
`shell-window-buffers', enlarge shell window to
`shell-window-normal-window-height'.")

(defvar shell-window-normal-window-height 20
  "Maximum hight of shell window.
But shell window can be higher if run `delete-other-window' when is is already
  to this height.")

(defvar shell-window-min-window-height (+ 1 window-min-height)
  "Minimum hight of shell window.")

(defun shell-window--scratch-buffer ()
  "Get the *scratch* buffer."
  (get-buffer "*scratch*"))

(defun shell-window--match-buffer (buffer-or-name)
  "Judge whether to use shell-window to show BUFFER-OR-NAME."
  (let ((buffer (get-buffer buffer-or-name)))
    (if buffer
        (let ((name (buffer-name buffer))
              (mode (buffer-local-value 'major-mode buffer)))
          (catch 'break
            (dolist (config shell-window-buffers)
              (let ((pattern (if (consp config) (car config) config))
                    (keywords (if (consp config) (cdr config) nil)))
                (if (cond ((eq pattern t) (not (buffer-file-name buffer)))
                          ((and (stringp pattern) (plist-get keywords :regexp))
                           (string-match pattern name))
                          ((stringp pattern)
                           (string= pattern name))
                          ((symbolp pattern)
                           (eq pattern mode))
                          ((functionp pattern)
                           (funcall pattern buffer))
                          (t (error "Invalid pattern: %s" pattern)))
                    (throw 'break config)))))))))

(defun shell-window--kill-buffer-when-shell-exit (&optional buffer)
  "Kill the buffer on exit of interactive shell.
if BUFFER is nil, use `current-buffer'."
  (let* ((buf (or buffer (current-buffer)))
         (process (get-buffer-process buf)))
    (if process
        (set-process-sentinel
         process
         #'(lambda (process state)
             (when (or (string-match "exited abnormally with code." state)
                       (string-match "\\(finished\\|exited\\)" state))
               (kill-buffer (process-buffer process))))))))

(defun shell-window--shrink-window (window)
  "Try to shrink shell WINDOW."
  (if shell-window-auto-resize
      (with-selected-window window
        (let ((size-fixed window-size-fixed))
          (setq window-size-fixed nil)
          (if (> (window-height window) shell-window-min-window-height)
              (minimize-window window))
          (setq window-size-fixed size-fixed)))))

(defun shell-window--enlarge-window (window &optional enforce-max-p)
  "Try to enlarge shell WINDOW.
If ENFORCE-MAX-P is not nil, try to maximize WINDOW."
  (if shell-window-auto-resize
      (with-selected-window window
        (let ((height-before (window-height window))
              (window-start (window-start))
              (window-end (window-end)))
          (when (or enforce-max-p (< (window-height window) shell-window-normal-window-height))
            (let ((max-delta (if enforce-max-p (window-max-delta window)
                               (- shell-window-normal-window-height height-before)))
                  (size-fixed window-size-fixed))
              (setq window-size-fixed nil)
              (if (window-resizable (selected-window) max-delta)
                  (window-resize (selected-window) max-delta))
              (setq window-size-fixed size-fixed)))

          ;; set shell window start
          (if (> (window-height window) height-before)
              (let ((forward-line-num (- height-before (window-height window)))
                    left-to-forward)
                (set-window-start
                 window
                 (let (return done)
                   (while (not done)
                     (save-excursion
                       (goto-char window-start)
                       (setq left-to-forward (forward-line forward-line-num))
                       (setq return (window-point window))
                       (if (or (eq window-end (window-end))
                               left-to-forward
                               (>= forward-line-num 0))
                           (setq done t)
                         (setq forward-line-num (+ forward-line-num 1)))))
                   return)
                 t)))))))

(defun shell-window--shell-window-p (window)
  "To judge whether WINDOW is shell window or not."
  (and window
       (windowp window)
       (window-parameter window 'window-side)
       (shell-window--match-buffer (window-buffer window))))

(defun shell-window--get-shell-window ()
  "Find shell window among all live windows.
If found, return the window, else return nil."
  (catch 'found
    (walk-window-tree
     (lambda (window)
       (if (shell-window--shell-window-p window)
           (throw 'found window))))))

;;; Minor Mode

(defun shell-window--display-buffer-condition (buffer-or-name _action)
  "Part of pair in `display-buffer-alist'.
BUFFER-OR-NAME is a buffer name to match, _ACTION is not used."
  (shell-window--match-buffer buffer-or-name))

(defun shell-window--display-buffer-action (buffer-or-name _alist)
  "Part of pair in `display-buffer-alist'.
BUFFER-OR-NAME is a buffer to display, _ALIST is not used."
  (let ((buffer (window-normalize-buffer-to-switch-to buffer-or-name))
        (window (shell-window--get-shell-window)))
    (if (and window (shell-window--match-buffer buffer-or-name))
        (set-window-dedicated-p window nil))
    (setq window (display-buffer-in-side-window buffer `((side . ,'bottom))))
    (set-window-dedicated-p window t)
    (select-window window)
    ;; (shell-window--enlarge-window window)
    ))

(defun shell-window--make-shell-buffer-list ()
  "Return shell buffer list that not shown in shell-window."
  (let ((visible-buffers (ido-get-buffers-in-frames 'current))
        (buffers (delq nil
                       (mapcar
                        (lambda (b)
                          (let ((name (buffer-name b)))
                            (if (shell-window--match-buffer b)
                                name)))
                        (buffer-list))))
        (current-buf (buffer-name (current-buffer))))
    ;; move visible buffers to start of buffer list
    (mapc #'(lambda (b)
              (when (member b buffers)
                (setq buffers (delq b buffers))
                (setq buffers (cons b buffers))))
          visible-buffers)
    ;; move current buffers to end of buffer list
    (when (member current-buf buffers)
      (setq buffers (delq current-buf buffers))
      (setq buffers (append buffers (list current-buf))))

    buffers))

(defun shell-window--display-and-select-buffer (buffer)
  "Display BUFFER in shell window and select the window."
  (let ((window (shell-window--get-shell-window)))
    (if (not window)
        (setq window (display-buffer-in-side-window (get-buffer buffer) `((side . ,'bottom)))))
    (set-window-dedicated-p window nil)
    (set-window-buffer window buffer)
    (set-window-dedicated-p window t)
    (select-window window)
    ;; (shell-window--enlarge-window window)
    ))

(defun shell-window--get-C-l-command ()
  "Get the command <tab> should be bound.
According to `major-mode' that yasnippet is not enabled and the
`global-map'."
  (let ((major-mode-map
         (symbol-value (intern-soft (concat (symbol-name major-mode) "-map")))))
    (or (and major-mode-map
             (lookup-key major-mode-map (kbd "C-l")))
        (lookup-key global-map (kbd "C-l")))))


(defun shell-window--auto-resize-hook ()
  "Resize shell window automatically.
If `shell-window-auto-resize' is t, switch out or in
`shell-window-buffers', shrink or enlarge shell window
automatically."
  (let ((window (shell-window--get-shell-window)))
    (when window
      (if (eq (selected-window) window)
          (shell-window--enlarge-window window)
        (shell-window--shrink-window window)))))

(defun shell-window--protect-scratch-buffer ()
  "Make *scratch* buffer unkillable."
  (let* ((shell-win (shell-window--get-shell-window))
         (scratch-buffer (shell-window--scratch-buffer))
         (window (selected-window))
         (buffer-to-kill (current-buffer))
         (hide-p (and (eq window shell-win)
                      (eq buffer-to-kill (window-buffer shell-win)))))
    (if (eq buffer-to-kill scratch-buffer) ;; not kill scratch buffer, clear it
        (progn (shell-window-clear-scratch-buffer buffer-to-kill) nil)
      (if hide-p (shell-window-hide))
      t)))

(defun shell-window-switch-buffer ()
  "Pop buffer that can be showed in shell-window.
This function get input by ido."
  (interactive)
  (let* ((shell-window-buffers (shell-window--make-shell-buffer-list))
         (chosen (and shell-window-buffers
                      (ido-completing-read "shell-window:" shell-window-buffers)))
         (buffer (and chosen
                      (not (string= chosen ""))
                      (get-buffer chosen))))
    (if (not buffer)
        (message (format "Buffer %s not exist" chosen))
      (shell-window--display-and-select-buffer buffer))))

(defun shell-window-create-scratch-buffer ()
  "Create a scratch buffer with the scratch message."
  (interactive)
  (unless (shell-window--scratch-buffer)
    (with-current-buffer (get-buffer-create "*scratch*")
      (progn
        (insert initial-scratch-message)
        (goto-char (point-max))
        (lisp-interaction-mode)
        (current-buffer)))))

(defun shell-window-clear-shell-buffer ()
  "Clear `eshell' or submode of `comint-mode' buffer."
  (interactive)
  (cond ((eq major-mode 'eshell-mode)
         (let ((eshell-buffer-maximum-lines 0))
           (eshell-truncate-buffer)))
        ((derived-mode-p 'comint-mode)
         (let ((comint-buffer-maximum-size 0))
           (comint-truncate-buffer)))
        (t (command-execute (shell-window--get-C-l-command)))))

(defun shell-window-clear-scratch-buffer (&optional buffer-or-name)
  "Clear the scratch buffer and keep the scratch message.
If BUFFER-OR-NAME is not nil, treat is as scratch buffer."
  (interactive)
  (let ((buffer (if (not buffer-or-name)
                    (shell-window--scratch-buffer)
                  (get-buffer buffer-or-name))))
    (if initial-scratch-message
        (with-current-buffer buffer
          (delete-region (point-min) (point-max))
          (insert initial-scratch-message)
          (set-buffer-modified-p nil)
          (goto-char (point-max))))))

(defadvice delete-other-windows (around shell-window-around-delete-other-windows)
  "If in shell window, just enlarge it instead of `delete-other-windows'."
  (let ((win-to-reserve (or (ad-get-arg 0) (selected-window)))
        (shell-window (shell-window--get-shell-window)))
    (if (eq win-to-reserve shell-window)
        ;; maximize shell window
        (shell-window--enlarge-window shell-window t)
      ad-do-it)))

;;;###autoload
(define-minor-mode shell-window-mode
  "Toggle shell-window minor mode.
Shell window is a window for showing shell like buffers, temp buffers and etc."
  :lighter " sw"
  :init-value nil
  :global t
  :group 'shell-window
  ;; The minor mode bindings.
  :keymap (make-sparse-keymap)
  (let ((pair '(shell-window--display-buffer-condition
                shell-window--display-buffer-action)))
    (if shell-window-mode
        (progn
          (push pair display-buffer-alist)

          (add-hook 'comint-mode-hook #'shell-window--kill-buffer-when-shell-exit)
          (add-hook 'kill-buffer-query-functions #'shell-window--protect-scratch-buffer)
          (add-hook 'buffer-list-update-hook #'shell-window--auto-resize-hook)

          (ad-activate #'delete-other-windows)

          (shell-window-create-scratch-buffer))

      ;; quite shell-window
      (setq display-buffer-alist (delete pair display-buffer-alist))

      (remove-hook 'comint-mode-hook #'shell-window--kill-buffer-when-shell-exit)
      (remove-hook 'kill-buffer-query-functions #'shell-window--protect-scratch-buffer)
      (remove-hook 'buffer-list-update-hook #'shell-window--auto-resize-hook)

      (ad-deactivate #'delete-other-windows)

      (shell-window-hide))))

(defun shell-window-hide ()
  "Hide shell window."
  (interactive)
  (if shell-window-mode
      (let ((window (shell-window--get-shell-window)))
        (if window
            (delete-window window)))
    (message "shell-window-mode is not enabled, do nothing")))

(provide 'shell-window)

;;; shell-window.el ends here
