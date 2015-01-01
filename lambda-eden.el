;;; lambda-eden.el --- JavaScript

;;; Commentary:

;;; Code:

(require 'lambda-core)

;;; TODO
;; evil-nerd-commenter
;; window-numbering
;; elnode emacs server
;; 研究 prelude purcell elpy srecode-mode, improve lambda-x according to prelude.

(if (eq emacs-major-version 24)
    (add-hook 'prog-mode-hook #'hs-minor-mode)
  (add-hook 'c-mode-common-hook #'hs-minor-mode))
;; vlf - view large file -------------------------------------------------------
(lambda-package-ensure-install 'vlf)
(require 'vlf-integrate)
;; iedit -----------------------------------------------------------------------
(lambda-package-ensure-install 'iedit)
;; fold about things -----------------------------------------------------------
(lambda-package-ensure-install 'fold-this)
;; ztree - compare directories -------------------------------------------------
(lambda-package-ensure-install 'ztree)
;; (push (substitute-in-file-name "path-to-ztree-directory") load-path)
(require 'ztree-dir)

(defun view-time (time-seconds)
  "Convert TIME-SECONDS from the epoch (0:00 January 1, 1970 UTC) to time string."
  (current-time-string (seconds-to-time time-seconds)))

;; ====================
;; insert date and time
(defvar current-date-time-format "%a %b %d %H:%M:%S %Z %Y"
  "Format of date to insert with `insert-current-date-time' func\
See help of `format-time-string' for possible replacements")

(defvar current-time-format "%a %H:%M:%S"
  "Format of date to insert with `insert-current-time' func.
Note the weekly scope of the command's precision.")

(defun insert-current-date-time ()
  "insert the current date and time into current buffer.
Uses `current-date-time-format' for the formatting the date/time."
       (interactive)
       (insert (format-time-string current-date-time-format (current-time))))

(defun insert-current-time ()
  "insert the current time (1-week scope) into the current buffer."
       (interactive)
       (insert (format-time-string current-time-format (current-time))))

;; (global-set-key "\C-c\C-d" 'insert-current-date-time)
;; (global-set-key "\C-c\C-t" 'insert-current-time)

;; to try
;; window-numbering evil-jumper Comment-dwim-2 evil-nerd-commenter

(provide 'lambda-eden)

;;; lambda-eden.el ends here
