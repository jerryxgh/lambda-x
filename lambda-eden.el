;;; lambda-eden.el --- A testing place -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'lambda-core)

;;; TODO
;; evil-nerd-commenter
;; window-numbering
;; elnode emacs server
;; 研究 prelude purcell elpy srecode-mode, improve lambda-x according to prelude.

(add-hook 'c-mode-common-hook #'hs-minor-mode)
;; vlf - view large file -------------------------------------------------------
(lambda-package-ensure-install 'vlf)
(require 'vlf-setup)
;; iedit -----------------------------------------------------------------------
(lambda-package-ensure-install 'iedit)

;; ztree - compare directories -------------------------------------------------
(lambda-package-ensure-install 'ztree)
;; (push (substitute-in-file-name "path-to-ztree-directory") load-path)
(require 'ztree-dir)

(defun view-time (time-seconds)
  "Convert TIME-SECONDS from the epoch (0:00 January 1, 1970 UTC) to time string."
  (current-time-string (seconds-to-time time-seconds)))

;; ====================
;; insert date or time
(defvar current-time-format "%T"
  "Format of date to insert with `insert-current-time' func.
Note the weekly scope of the command's precision.")

(defun insert-current-date ()
  "Insert current date in iso 8601 format into current buffer."
  (interactive)
  (insert (concat
           (format-time-string "%Y-%m-%d"))))

(defun insert-current-date-time ()
  "Insert current date and time in iso 8601 format into current buffer."
  (interactive)
  (insert (concat
           (format-time-string "%Y-%m-%dT%T")
           (funcall (lambda (x)
                      (concat (substring x 0 3) ":" (substring x 3 5)))
                    (format-time-string "%z")))))

(defun insert-current-time ()
  "Insert the current time (1-week scope) into the current buffer."
  (interactive)
  (insert (format-time-string current-time-format (current-time) t)))

;; (global-set-key "\C-c\C-d" 'insert-current-date-time)
;; (global-set-key "\C-c\C-t" 'insert-current-time)

;; to try
;; window-numbering evil-jumper Comment-dwim-2 evil-nerd-commenter

(defun lambda-copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(require 'smartwin)
(smartwin-mode 1)


;;; es-mode
(lambda-package-ensure-install 'es-mode)

;; template engine
(lambda-package-ensure-install 'mustache)
(lambda-package-ensure-install 'f)

(lambda-package-ensure-install 'nginx-mode)
(require 'nginx-mode)
(add-to-list 'auto-mode-alist
             '("modsecurity\.conf$" . nginx-mode)
             '("modsecurity\.conf$" . nginx-mode))

;; knowledge
;; locate-library
;; list-load-path-shadows

(provide 'lambda-eden)

;;; lambda-eden.el ends here
