;;; lambda-eden.el --- A testing place -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'lambda-core)

;;; TODO
;; elnode emacs server
;; 研究 prelude purcell elpy srecode-mode, improve lambda-x according to prelude.

;; vlf - view large file -------------------------------------------------------
(use-package vlf
  :ensure t)

;; iedit -----------------------------------------------------------------------
(use-package iedit
  :ensure t)

;; ztree - compare directories -------------------------------------------------
(use-package ztree
  :ensure t)
;; (push (substitute-in-file-name "path-to-ztree-directory") load-path)
;; (require 'ztree-dir)

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
           (format-time-string "%Y-%m-%d %T"))))

(defun insert-current-time ()
  "Insert the current time (1-week scope) into the current buffer."
  (interactive)
  (insert (format-time-string current-time-format (current-time) t)))

;; (global-set-key "\C-c\C-d" 'insert-current-date-time)
;; (global-set-key "\C-c\C-t" 'insert-current-time)

(defun lambda-copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

;; template engine
(use-package mustache
  :ensure t)
(use-package f
  :ensure t)

(use-package nginx-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist
               '("modsecurity\.conf$" . nginx-mode)))

(use-package git-messenger
  :ensure t)

;; trying
;; web servers
(use-package simple-httpd
  :ensure t)

;; knowledge
;; locate-library
;; list-load-path-shadows

(use-package better-jumper
  :config
  (better-jumper-mode +1)
  (with-eval-after-load 'evil-maps
    (define-key evil-motion-state-map (kbd "C-o") 'better-jumper-jump-backward)
    (define-key evil-motion-state-map (kbd "C-i") 'better-jumper-jump-forward)))

;;; svn - psvn =================================================================
(use-package magit-svn
  :ensure t
  :hook (magit-mode . magit-svn-mode))

;; fasd ========================================================================
(require 'fasd-shell)
(add-hook 'shell-mode-hook 'fasd-shell-mode)

;; whitespace-cleanup-mode =====================================================
(use-package whitespace-cleanup-mode
  :ensure t
  :config
  (global-whitespace-cleanup-mode 1))

(use-package yaml-mode
  :ensure t)

;; show free key bindings
(use-package free-keys
  :ensure t)

(use-package package-lint
  :ensure t
  :pin melpa
  )

(provide 'lambda-eden)

;;; lambda-eden.el ends here
