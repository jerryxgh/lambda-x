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
  :delight
  :config
  (global-whitespace-cleanup-mode 1))

(use-package yaml-mode
  :ensure t)

;; show free key bindings
(use-package free-keys
  :ensure t)

(use-package package-lint
  :ensure t
  :pin melpa)

;; straight straight.el is a replacement for package.el, not
;; use-package. use-package can be used with either package.el or straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


;; (use-package copilot
;;   :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
;;   :ensure t)

;; (add-hook 'prog-mode-hook 'copilot-mode)
;; (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
;; (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))

(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

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

(use-package package-lint
  :ensure t
  :config
  (require 'package-lint-flymake)
  (add-hook 'emacs-lisp-mode-hook #'package-lint-flymake-setup))

(defun lambda-term-keys-want-key-p-def (key mods)
  "Lambda implementation for `term-keys/want-key-p-func'.

This function controls which key combinations are to be encoded
and decoded using the term-keys protocol extension.
KEY is the KeySym name as listed in `term-keys/mapping'; MODS is
a 6-element bool vector representing the modifiers Shift /
Control / Meta / Super / Hyper / Alt respectively, with t or nil
representing whether they are depressed or not.  Returns non-nil
if the specified key combination should be encoded.

Note that the ALT modifier rarely actually corresponds to the Alt
key on PC keyboards; the META modifier will usually be used
instead."
  (let ((shift   (elt mods 0))
        (control (elt mods 1))
        (meta    (elt mods 2))
        (super   (elt mods 3))
        (hyper   (elt mods 4))
        (alt     (elt mods 5)))
    (and

     ;; We don't care about Super/Hyper/Alt modifiers
     (not super)
     (not hyper)
     (not alt)

     (or
      ;; Navigation keys and Control/Alt
      (and (member key '("Left" "Right")) control)
      ))))

;; use command below to generate alacritty config, convert to toml like this:
;; { key = "Left", mods = "Command", chars = "\u001b\u001f\u0054\u0062\u001f" },
;; { key = "Left", mods = "Command | Shift", chars = "\u001b\u001f\u0054\u0063\u001f" },
;; { key = "Right", mods = "Command", chars = "\u001b\u001f\u0055\u0042\u001f" },
;; { key = "Right", mods = "Command | Shift", chars = "\u001b\u001f\u0055\u0043\u001f" },

;; (require 'term-keys-alacritty)
;; (with-temp-buffer
;;   (insert (term-keys/alacritty-config))
;;   (write-region
;;    (point-min) (point-max) "~/alacritty-for-term-keys.yml"))

(use-package term-keys
  :ensure t
  :custom
  (term-keys/want-key-p-func 'lambda-term-keys-want-key-p-def)
  :config
  (term-keys-mode t))

(provide 'lambda-eden)

;;; lambda-eden.el ends here
