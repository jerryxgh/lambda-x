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

;; template engine
(use-package mustache
  :ensure t)
(use-package f
  :ensure t)

(use-package nginx-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist
               '("\\(?:\\`\\|/\\)modsecurity\\.conf\\'" . nginx-mode)))

(use-package git-messenger
  :ensure t)

;; trying
;; web servers
(use-package simple-httpd
  :ensure t)

;; knowledge
;; locate-library
;; list-load-path-shadows

;;; svn - psvn =================================================================
(use-package magit-svn
  :ensure t
  :hook (magit-mode . magit-svn-mode))

;; whitespace-cleanup-mode =====================================================
(defun lambda-whitespace-cleanup-mode-maybe ()
  "Enable whitespace cleanup unless the current buffer is large or uses VLF."
  (unless (or (bound-and-true-p vlf-mode)
              (and (local-variable-p 'vlf-file-size)
                   (> vlf-file-size 0))
              (and large-file-warning-threshold
                   (> (buffer-size) large-file-warning-threshold)))
    (whitespace-cleanup-mode 1)))

(use-package whitespace-cleanup-mode
  :ensure t
  :delight
  :hook (prog-mode . lambda-whitespace-cleanup-mode-maybe))

(use-package yaml-mode
  :ensure t)

;; show free key bindings
(use-package free-keys
  :ensure t)

;; (use-package copilot
;;   :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
;;   :ensure t)

;; (add-hook 'prog-mode-hook 'copilot-mode)
;; (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
;; (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)

(use-package fzf
  :bind
    ;; Don't forget to set keybinds!
  :config
  (setq fzf/args "-x --color bw --print-query --margin=1,0 --no-hscroll"
        fzf/executable "fzf"
        fzf/git-grep-args "-i --line-number %s"
        ;; command used for `fzf-grep-*` functions
        ;; example usage for ripgrep:
        ;; fzf/grep-command "rg --no-heading -nH"
        fzf/grep-command "grep -nrH"
        ;; If nil, the fzf buffer will appear at the top of the window
        fzf/position-bottom t
        fzf/window-height 15))

(use-package vimrc-mode
  :ensure t
  :config
  (require 'vimrc-mode)
  (add-to-list 'auto-mode-alist '("\\.vim\\(rc\\)?\\'" . vimrc-mode)))

(use-package agent-shell
  :config
  ;; Evil state-specific RET behavior: insert mode = newline, normal mode = send
  (evil-define-key 'insert agent-shell-mode-map (kbd "RET") #'newline)
  (evil-define-key 'normal agent-shell-mode-map (kbd "RET") #'comint-send-input)

  ;; Configure *agent-shell-diff* buffers to start in Emacs state
  (add-hook 'diff-mode-hook
            (lambda ()
              (when (string-match-p "\\*agent-shell-diff\\*" (buffer-name))
                (evil-emacs-state)))))

(provide 'lambda-eden)

;;; lambda-eden.el ends here
