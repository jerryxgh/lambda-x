;; lambda-core.el --- core settings, shared by all other modules

;;; Commentary:
;; Core settings, shared by all other modules.

;;; Code:

;; Maxmize frame ---------------------------------------------------------------
;; fullscreen when startup finished
;; (custom-set-variables
;;  '(initial-frame-alist (quote ((fullscreen . fullboth)))))

;; (defun lambda-maxmize-frame ()
;;   "Make Emacs frame maxmized."
;;   (interactive)
;;   (cond ((and (eq system-type 'windows-nt)
;;               (fboundp 'w32-send-sys-command))
;;          (w32-send-sys-command 61488))
;;         ((eq system-type 'gnu/linux)
;;          (set-frame-parameter nil 'fullscreen 'maximized))
;;         (t
;;          (set-frame-parameter nil 'fullscreen 'maximized))))

;; (add-hook 'after-init-hook 'lambda-maxmize-frame)

(require 'lambda-package)
(require 'dired)

(defconst current-user
  (getenv
   (if (equal system-type 'windows-nt) "USERNAME" "USER"))
  "Current user name.")

(defconst lambda-auto-save-dir (expand-file-name "auto-save-list/"
                                                 user-emacs-directory)
  "This folder stores all the automatically generated save/history-files.")

(defun lambda-add-to-load-path-recursively
    (directory &optional exclude-directories-list)
  "Add DIRECTORY to `load-path' recursively, those has `get-load-suffixes' file.
If a directory name is one of EXCLUDE-DIRECTORIES-LIST, then this directory and
 subdirectry will be excluded."
  (unless (file-directory-p directory)
    (setq directory (file-name-directory directory)))
  (let (directory-stack (suffixes (get-load-suffixes)))
    (if (file-exists-p directory)
        (push directory directory-stack))
    ;; use stack to traverse directory and subdirectory
    (while directory-stack
      (let* ((current-directory (pop directory-stack))
             (file-list (directory-files current-directory t))
             should-add-to-load-path)

        (dolist (file file-list)
          (let ((file-name (file-name-nondirectory file)))
            (if (file-directory-p file)
                ;; exclude . .. and exclude-directories-list
                (if (and (not (equal file-name "."))
                         (not (equal file-name ".."))
                         (not (member file-name exclude-directories-list)))
                    (push file directory-stack))
              (if (and (not should-add-to-load-path)
                       (member (file-name-extension file t) suffixes))
                  (setq should-add-to-load-path t)))))
        (if should-add-to-load-path
            (add-to-list 'load-path current-directory))))))

(lambda-add-to-load-path-recursively (expand-file-name "packages/non-elpa"
                                                       lambda-package-direcotry))

(require 'lambda-widget)

;; suppressing ad-handle-definition Warnings in Emacs
(setq ad-redefinition-action 'accept)

;; init PATH in mac, this should just after packages settings ==================
(when (eq system-type 'darwin)
  ;; open file in Find will reuse current frame instead creating a new one

  ;; use command as control
  (setq ns-command-modifier 'control)
  (setq ns-pop-up-frames nil)
  (use-package exec-path-from-shell
    :ensure t)

  (if (memq window-system '(mac ns))
      (exec-path-from-shell-initialize)))
;; init PATH in mac ends here ==================================================

;; Emacs UI about settings =====================================================

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; diminish keeps the modeline tidy, this package is needed by others, so put
;; this at the beginning -------------------------------------------------------
(use-package diminish
  :ensure t)

(use-package delight
  :ensure t)

;; toolbar is just a waste of valuable screen estate in a tty tool-bar-mode does
;; not properly auto-load, and is already disabled anyway
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; disable startup screen
(setq inhibit-startup-screen t)

;; nice scrolling in both keyboard and mouse
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)) ; three line at a time
      mouse-wheel-progressive-speed nil ; donnot accelerate scrolling
      ;; scroll-step 1
      ;; scroll-margin 0
      scroll-preserve-screen-position t
      scroll-margin 0
      scroll-conservatively 101)

;; resize windows in pixel
(setq window-resize-pixelwise t)

;; mode line settings
(line-number-mode t)
(setq line-number-display-limit-width 1000000000)
(column-number-mode t)
(size-indication-mode t)

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '("[" invocation-name " lambda-x] - "
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name)) "%b"))))

;; show line numbers left side when in prog mode
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(use-package display-line-numbers
  :ensure nil
  :custom
  (display-line-numbers-grow-only t)
  (display-line-numbers-width-start 100))

;; set language environment ----------------------------------------------------
;; Contrary to what many Emacs users have in their configs, you don't need
;; more than this to make UTF-8 the default coding system:
(set-locale-environment "en_US.UTF-8")
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(setq system-time-locale "C") ; 这个是为了在 org mode 中用英文显示日期，默认是中文

;; theme -----------------------------------------------------------------------

(if (display-graphic-p)
    ;;; if graphic
    (progn
      ;; fix title bar text color broken: https://github.com/d12frosted/homebrew-emacs-plus/issues/55
      (when (and(eq system-type 'darwin) (> emacs-major-version 26))
        (add-to-list 'default-frame-alist '(ns-appearance . dark)))

      )
  ;;; else (terminal)
  ;; close menu bar
  (menu-bar-mode -1)

  ;; use command as control
  (setq ns-command-modifier 'control))

(defun lambda-load-theme (theme)
  "Load THEME, plus that, set font and tweak mode-line style."
  ;; make font in the vertical middle of line
  (setq-default default-text-properties '(line-spacing 3 line-height 18))

  (load-theme theme t)

  (cond ((eq system-type 'windows-nt)
         (set-frame-font "Consolas-11")
         (set-face-attribute 'default nil :font "Consolas-11")
         (set-face-attribute 'default t :font "Consolas-11")
         (set-face-attribute 'mode-line nil :font "Consolas-11"))

        ((eq system-type 'gnu/linux)
         (set-frame-font "Source Code Pro-11")
         (if (fboundp 'set-fontset-font)
             (set-fontset-font t 'unicode '("Noto Sans CJK SC" .
                                            "unicode-bmp")))
         ;; (setq face-font-rescale-alist (list (cons "Noto Sans CJK SC" 1.2)))
         (setq face-font-rescale-alist (list (cons "Noto Sans CJK SC" 1.0))))

        ((eq system-type 'darwin)
         (set-frame-font "menlo-14")
         (set-fontset-font "fontset-default" 'han '("PingFang SC"))
         ;; (setq face-font-rescale-alist (list (cons "PingFang SC" 1.2)))
         (setq face-font-rescale-alist (list (cons "PingFang SC" 1.0)))
         )))

;; (lambda-package-ensure-install 'spacemacs-theme)
;; (lambda-load-theme 'spacemacs-dark)

;; (use-package solarized-theme
;;   :ensure
;;   :config
;;   (lambda-load-theme 'solarized-dark)
;;   )

;; highlight current line
(global-hl-line-mode t)

(use-package zenburn-theme
  :ensure t
  :config
  ;; use variable-pitch fonts for some headings and titles
  (setq zenburn-use-variable-pitch t)
  ;; scale headings in org-mode
  (setq zenburn-scale-org-headlines t)
  ;; scale headings in outline-mode
  (setq zenburn-scale-outline-headlines t)
  (lambda-load-theme 'zenburn)
  (unless (display-graphic-p)
    (require 'hl-line)
    (set-face-background 'hl-line "#333333")))

;; winum
(defun window-numbering-install-mode-line (&optional position)
  "Do nothing, the display is handled by the spaceline(powerline).
POSITION: just inhibit warning.")

(use-package winum
  :ensure t
  :custom
  ;; do not show winum in mode line, let spaceline do it
  (winum-format "")
  :config
  (winum-set-keymap-prefix (kbd "C-w"))
  (winum-mode))

;; mode line theme -------------------------------------------------------------
(use-package powerline
  :ensure t
  :custom
  (powerline-height 23))

(use-package spaceline
  :ensure t
  :config
  (setq spaceline-window-numbers-unicode t
        spaceline-workspace-numbers-unicode t
        powerline-default-separator 'bar
        ;; different color for different evil state in modeline
        spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
  ;; (spaceline-spacemacs-theme '(buffer-encoding process))
  ;; this require is for spaceline-spacemacs-theme on the first installation of spaceline
  (require 'spaceline-config)
  (spaceline-spacemacs-theme)
  ;; (spaceline-helm-mode)
  (redisplay))

;;; which-key --------------------------------------------------------------------
;; optional if you want which-key integration
(use-package which-key
  :ensure t
  :delight which-key-mode
  :config
  ;; Allow C-h to trigger which-key before it is done automatically
  (setq which-key-show-early-on-C-h t)
  ;; make sure which-key doesn't show normally but refreshes quickly after it is
  ;; triggered.
  (setq which-key-idle-delay 10000)
  (setq which-key-idle-secondary-delay 0.05)
  (which-key-mode)
  (which-key-setup-side-window-bottom))

;; inhibit annoying warning sound
(setq ring-bell-function 'ignore)

(mouse-avoidance-mode 'none)
;; (show-paren-mode -1)
;; (setq show-paren-style 'mixed)
;; (set-background-color "#CCE8CF")

(setq-default indicate-buffer-boundaries '((top . left) (t . right))
              indicate-empty-lines t)

;; use minibuffer instead of dialog to ask questions
(setq use-dialog-box nil)

;; for woman
(setq woman-fill-frame t)

;; Emacs UI about settings end here ===========================================

;; editor settings ============================================================
;; confirm when quit emacs
(use-package files
  :ensure nil
  :custom
  (confirm-kill-emacs 'y-or-n-p))

;; auto insert newline at end of file if it has none
(setq-default require-final-newline nil)

;; directory to store all backup and autosave files
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode 1)
(diminish 'auto-revert-mode)
(electric-indent-mode 1)

;; hippie expand is dabbrev expand on steroids
(use-package hippie-exp
  :ensure nil
  :custom
  (hippie-expand-try-functions-list '(try-expand-dabbrev
                                      try-expand-dabbrev-all-buffers
                                      try-expand-dabbrev-from-kill
                                      try-complete-file-name-partially
                                      try-complete-file-name
                                      try-expand-all-abbrevs
                                      try-expand-list
                                      try-expand-line
                                      try-complete-lisp-symbol-partially
                                      try-complete-lisp-symbol)))

;; abbrev-mode settings
(setq abbrev-file-name (expand-file-name "abbrev_defs" lambda-auto-save-dir))
(setq-default abbrev-mode t)
(diminish 'abbrev-mode)
(if (file-exists-p abbrev-file-name)
    (quietly-read-abbrev-file))

;; smartparens -----------------------------------------------------------------
(use-package smartparens
  :ensure t
  :custom
  (sp-autoskip-closing-pair 'always)
  (sp-base-key-bindings 'sp)
  (sp-override-key-bindings
     '(("C-S-<left>" . sp-backward-slurp-sexp)
       ("C-S-<right>" . sp-backward-barf-sexp)
       ("M-<delete>" . nil)
       ("M-<backspace>" . nil)
       ))
  :config
  (smartparens-global-mode t)
  (smartparens-global-strict-mode t)
  (show-smartparens-global-mode t)
  (diminish 'smartparens-mode)
   ;; load default config
  (require 'smartparens-config))

;; uniquify --- easy to distinguish same name buffers
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

;; use shift + arrow keys to switch between visible buffers
(use-package windmove
  :ensure t
  :custom
  (windmove-allow-all-windows t)
  :config
  (require 'windmove)
  (windmove-default-keybindings))

;;; tramp
;; usage: type `C-x C-f' and then enter the filename`/user@machine:/path/to.file
(require 'tramp)
(require 'tramp-cache)
(setq tramp-auto-save-directory  temporary-file-directory)
(setq tramp-persistency-file-name (expand-file-name "tramp"
                                                    lambda-auto-save-dir))
(if (eq system-type 'windows-nt)
    (setq tramp-default-method "plink")
  (setq tramp-default-method "ssh"))
(when (> emacs-major-version 23)
  (require 'tramp-sh)
  (delete "LC_ALL=C" tramp-remote-process-environment)
  (add-to-list 'tramp-remote-process-environment "LANG=zh_CN.UTF-8" 'append)
  (add-to-list 'tramp-remote-process-environment "LC_ALL=\"zh_CN.UTF-8\""
               'append))

;;; imenu
(set-default 'imenu-auto-rescan t)

;; flyspell
(require 'flyspell)
(if (eq system-type 'windows-nt)
    (add-to-list 'exec-path "C:/Program Files (x86)/Aspell/bin/"))
(setq ispell-program-name "aspell"
      ispell-extra-args '("--sug-mode=ultra"))
(setq ispell-personal-dictionary (expand-file-name "ispell" lambda-package-direcotry))

;; enable narrowing commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;; enabled change region case commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; enable erase-buffer command
(put 'erase-buffer 'disabled nil)

;; dired - reuse current buffer by pressing 'a'
(put 'dired-find-alternate-file 'disabled nil)

;; How to sort directories first in dired: https://emacs.stackexchange.com/questions/29096/how-to-sort-directories-first-in-dired
(require 'ls-lisp)
(setq ls-lisp-dirs-first t
      ls-lisp-use-insert-directory-program nil)

;;; dired-x
(require 'dired-x)
(cond ((eq system-type 'windows-nt)
       (setq dired-listing-switches "-Al"))
      ((eq system-type 'darwin)
       (if (executable-find "gls")
           (progn
             (setq insert-directory-program (executable-find "gls"))
             (setq dired-listing-switches "-Al --group-directories-first"))
         (setq dired-listing-switches "-Al")))
      (t (setq dired-listing-switches "-Al --group-directories-first")))

(use-package dired
  :ensure nil
  :custom
  (dired-dwim-target t))

;;; bookmark -------------------------------------------------------------------
(use-package bookmark
  :ensure nil
  :custom
  (bookmark-default-file (expand-file-name "bookmarks"
                                           lambda-auto-save-dir)))

;; projectile is a project management mode -------------------------------------
(use-package projectile
  :ensure t
  ;; :delight projectile-mode
  :custom
  ;; (projectile-mode-line-function '(lambda () (format " [%s]" (projectile-project-name))))
  (projectile-enable-caching t)
  (projectile-file-exists-remote-cache-expire nil)
  (projectile-completion-system 'ivy)
  (projectile-cache-file (expand-file-name
                          "projectile.cache"
                          lambda-auto-save-dir))
  (projectile-known-projects-file (expand-file-name
                                   "projectile-bookmarks.eld"
                                   lambda-auto-save-dir))
  (projectile-ignored-project-function (lambda (project-root)
                                         (lambda-x-under-gvm-directory-p project-root)))

  :config
  (defadvice projectile-project-files (around projectile-project-files-advice)
    "Do not load file list when under specific directory"
    (if (lambda-x-under-gvm-directory-p (ad-get-arg 0))
        nil
      ad-do-it))
  (ad-activate 'projectile-project-files)

  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode t))

(defun lambda-x-under-gvm-directory-p (project-root)
  "When PROJECT-ROOT is under ~/.gvm directory, do not add it to projectile."
  (let ((gvm-dir (expand-file-name "~/.gvm"))
        (true-gvm-dir (file-truename "~/.gvm"))
        (project-root-dir (expand-file-name project-root))
        (true-project-root-dir (file-truename project-root)))

    (or (string-prefix-p gvm-dir project-root-dir)
        (string-prefix-p true-gvm-dir project-root-dir)
        (string-prefix-p gvm-dir true-project-root-dir)
        (string-prefix-p true-gvm-dir true-project-root-dir))))

(use-package ibuffer-projectile
  :ensure t
  :config
  (add-hook 'ibuffer-hook
    (lambda ()
      (ibuffer-projectile-set-filter-groups)
      (unless (eq ibuffer-sorting-mode 'alphabetic)
        (ibuffer-do-sort-by-alphabetic)))))

;; anzu-mode enhances isearch by showing total matches and current match
;; position --------------------------------------------------------------------
(use-package anzu
  :ensure t
  :custom
  (anzu-cons-mode-line-p nil)
  :config
  (global-anzu-mode 1)
  (diminish 'anzu-mode))

;; ediff - don't start another frame
(require 'ediff)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(defvar ediff-saved-window-configuration nil
  "Window configuration before ediff.")
(add-hook 'ediff-before-setup-hook
          #'(lambda ()
              (setq ediff-saved-window-configuration
                    (current-window-configuration))))
(add-hook 'ediff-quit-hook
          #'(lambda ()
              (set-window-configuration
               ediff-saved-window-configuration))
          'append)
(add-hook 'ediff-suspend-hook
          #'(lambda ()
              (set-window-configuration
               ediff-saved-window-configuration))
          'append)

;; clean up obsolete buffers automatically
(require 'midnight)
(setq midnight-period 7200) ;; (eq (* 2 60 60) "2 hours")

;; saner regex syntax
(require 're-builder)
(setq reb-re-syntax 'string)

(require 'compile)
(setq compilation-ask-about-save nil  ; Just save before compiling
      ;; just kill old compile processes before starting the new one
      compilation-always-kill t
      ;; automatically scroll to first error
      compilation-scroll-output 'first-error)

;; let one line display as one line, even if it over the window
(setq-default truncate-lines t)
(setq-default indent-tabs-mode nil)
;; set text-mode as the default major mode, instead of fundamental-mode
(setq-default major-mode 'text-mode)

;; enable winner-mode to manage window configurations
(winner-mode 1)

;; editor settings end here ====================================================

;; miscellaneous basic settings ------------------------------------------------
(setq custom-file (expand-file-name "lambda-custom.el" lambda-package-direcotry)
      make-backup-files nil
      resize-mini-windows t
      enable-recursive-minibuffers t
      gc-cons-threshold 20480000)


(if (string< emacs-version "24.3.50")
    (diminish 'global-visual-line-mode))
(diminish 'visual-line-mode)
;; enable to support navigate in camelCase words
(auto-compression-mode t)
(auto-image-file-mode t)

;; enable hs-minor-mode for programming, hide or show code
(add-hook 'prog-mode-hook
          (lambda ()
            (hs-minor-mode t)
            (diminish 'hs-minor-mode)))

(defmacro with-region-or-buffer (func)
  "When called with no active region, call FUNC on current buffer."
  `(defadvice ,func (before with-region-or-buffer activate compile)
     (interactive
      (if mark-active
          (list (region-beginning) (region-end))
        (list (point-min) (point-max))))))

(with-region-or-buffer indent-region)
;; (with-region-or-buffer untabify)

;;; ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)
;;(setq ibuffer-never-show-predicates (list "^ ?\\*.*\\*$"))

;;; time-stamp
(require 'time-stamp)
(setq time-stamp-active t
      time-stamp-warn-inactive t
      time-stamp-format "%:y-%02m-%02d %02H:%02M:%02S %U")
(add-hook 'before-save-hook 'time-stamp)

;;; easypg
(require 'epa)
(require 'epa-file)
(epa-file-enable)
(setq epa-file-encrypt-to nil
      epa-file-select-keys 'silent
      epa-file-cache-passphrase-for-symmetric-encryption t
      epa-file-inhibit-auto-save t)
(setenv "GPG_AGENT_INFO" nil) ; use minibuffer to input passphrase

;; flymake - try best to use built-in package
(use-package flymake
  :custom
  (flymake-start-on-save-buffer t)
  :config
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)
  (with-eval-after-load 'prog-mode
    (with-eval-after-load 'flymake
      (define-key prog-mode-map (kbd "M-n") 'flymake-goto-next-error)
      (define-key prog-mode-map (kbd "M-p") 'flymake-goto-prev-error)
      (with-eval-after-load 'go-mode
        (define-key go-dot-mod-mode-map (kbd "M-n") 'flymake-goto-next-error)
        (define-key go-dot-mod-mode-map (kbd "M-p") 'flymake-goto-prev-error)))))

;; flycheck - much better than flymake -----------------------------------------
;; (lambda-package-ensure-install 'flycheck)
(use-package flycheck
  :ensure
  ;; enable on-the-fly syntax checking
  :init (global-flycheck-mode 1)
  :custom
  (flycheck-global-modes '(not sql-mode go-mode java-mode c-mode c++-mode cc-mode python-mode emacs-lisp-mode))
  (compilation-skip-threshold 0)
  (flycheck-emacs-lisp-initialize-packages t)
  (flycheck-emacs-lisp-package-user-dir package-user-dir)
  ;; use spaceline to show flycheck status instead
  (flycheck-mode-line nil))

(use-package flycheck-package
  :ensure
  :config
  (flycheck-package-setup))

(use-package emacs
  :custom
  ;; (mode-require-final-newline nil) ; do't auto insert new line when saving file
  (switch-to-buffer-in-dedicated-window 'pop))

;; sensible undo ---------------------------------------------------------------
(use-package undo-tree
  :ensure t
  :config
  (require 'undo-tree)
  ;; autosave the undo-tree history
  (setq undo-tree-history-directory-alist
        `((".*" . ,temporary-file-directory))
        undo-tree-auto-save-history nil
        ;; https://github.com/syl20bnr/spacemacs/issues/9903
        undo-tree-enable-undo-in-region nil
        ;; undo-tree-visualizer-timestamps t
        )
  (global-undo-tree-mode 1)
  (diminish 'undo-tree-mode))

;;temp-buffer-browse -----------------------------------------------------------
(use-package temp-buffer-browse
  :ensure t
  :config
  (require 'temp-buffer-browse)
  (temp-buffer-browse-mode 1))

;; shell configs ---------------------------------------------------------------
(require 'shell)
(require 'esh-mode)

(define-key shell-mode-map (kbd "C-j") 'comint-send-input)

;; close *compilation* buffer when compilation success
;; (add-hook 'compilation-start-hook 'kill-buffer-when-shell-command-exit)
;; always insert at the bottom of shell like buffers
(setq comint-scroll-to-bottom-on-input t)
;; (setq shell-cd-regexp "")
;; no duplicates in command history
(setq comint-input-ignoredups t)
(defun lambda-comint-init ()
  "Prevent shell commands from being echoed."
  (setq comint-process-echoes t))
(add-hook 'comint-mode-hook 'lambda-comint-init)

;; make a shell script executable automatically on save
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)
;; .zsh file is shell script too
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . shell-script-mode))

;; eshell configs --------------------------------------------------------------
(require 'eshell)
(setq eshell-directory-name (expand-file-name
                             "eshell"
                             lambda-auto-save-dir))
;; when input things in eshell, goto the end of the buffer automatically
(setq eshell-scroll-to-bottom-on-input 'this)
(add-hook 'eshell-mode-hook
          #'(lambda ()
              (eldoc-mode 1)
              (define-key eshell-mode-map (kbd "C-j") 'eshell-send-input)))

;; do not load custom file, all the configuration should be done by code
(load "lambda-custom" t)

;;; magit --- use git in emacs--------------------------------------------------
(use-package transient
  :ensure t
  :pin melpa-stable
  :custom
  (auto-revert-buffer-list-filter 'magit-auto-revert-repository-buffer-p)
  (transient-display-buffer-action
   '(display-buffer-below-selected
    (dedicated . t)
    (inhibit-same-window . t)
    (window-parameters (no-other-window . t)))))

(use-package magit
  :ensure t
  :pin melpa-stable
  ;; :bind ("C-c g" . magit-status)
  :custom
  (split-height-threshold 98)                          ; try to split vertically
  (ediff-split-window-function 'split-window-sensibly) ; try to split vertically in ediff
  (magit-refresh-status-buffer nil)
  (magit-ediff-dwim-show-on-hunks t)
  (magit-bury-buffer-function (lambda (&rest _)
                                (let* ((buf (window-buffer (selected-window)))
                                       (status-buf (magit-get-mode-buffer 'magit-status-mode))
                                       (is-quit-status-buf (eq buf status-buf)))
                                  (magit-mode-quit-window 1)
                                  (if is-quit-status-buf
                                      (mapc #'kill-buffer (magit-mode-get-buffers))))))
  :init
  (use-package with-editor :ensure t)

  :config
  ;; windows support
  (let ((git-executable-windows "C:/Program Files (x86)/Git/bin/git.exe"))
    (when (and (eq system-type 'windows-nt)
               (file-exists-p git-executable-windows))
      (setq magit-git-executable git-executable-windows)
      (setenv "PATH"
              (concat (getenv "PATH") ";c:/Program Files (x86)/Git/bin/"))))

  (defun kill-magit-diff-buffer-in-current-repo (&rest _)
    "Delete the magit-diff buffer related to the current repo"
    (let ((magit-diff-buffer-in-current-repo (magit-get-mode-buffer 'magit-diff-mode))
          (magit-meta-diff-buffer-in-current-repo (magit-get-mode-buffer 'ediff-meta-mode)))
      (kill-buffer magit-diff-buffer-in-current-repo)))
  ;;
  ;; When 'C-c C-c' or 'C-c C-l' are pressed in the magit commit message buffer,
  ;; delete the magit-diff buffer related to the current repo.
  ;;
  (add-hook 'git-commit-setup-hook
            (lambda ()
              (add-hook 'with-editor-post-finish-hook #'kill-magit-diff-buffer-in-current-repo
                        nil t)
              (add-hook 'with-editor-post-cancel-hook #'kill-magit-diff-buffer-in-current-repo
                        nil t)))
  ;; (remove-hook 'magit-status-sections-hook 'magit-insert-tags-header)
  ;; (remove-hook 'magit-status-sections-hook 'magit-insert-status-headers)
  ;; (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-pushremote)
  ;; (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-pushremote)
  ;; (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-upstream)
  ;; (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-upstream-or-recent)
  )

;;; fill-column ----------------------------------------------------------------
(setq-default fill-column 80)
(lambda-package-ensure-install 'fill-column-indicator)
(add-hook 'prog-mode-hook #'(lambda ()
                              (turn-off-auto-fill)))
;; mode names typically end in "-mode", but for historical reasons
;; auto-fill-mode is named by "auto-fill-function".
(diminish 'auto-fill-function)

;; key-bindings for myself, you can change this to yours -----------------------
(global-set-key (kbd "C-x k") 'kill-this-buffer)

(setq mouse-wheel-scroll-amount '(3 ((shift) . 1)) ; three line at a time
      mouse-wheel-progressive-speed nil ; don't accelerate scrolling
      scroll-step 1
      scroll-preserve-screen-position t
      scroll-conservatively most-positive-fixnum)

;; YASnippet -------------------------------------------------------------------
(lambda-package-ensure-install 'yasnippet)
(lambda-package-ensure-install 'yasnippet-snippets)
(require 'yasnippet)
(add-to-list 'yas-snippet-dirs (expand-file-name "snippets" lambda-package-direcotry))
;; delete dirs that not exist in yas-snippet-dirs
(dolist (dir yas-snippet-dirs)
  (if (stringp dir)
      (unless (file-directory-p dir)
        (setq yas-snippet-dirs (delete dir yas-snippet-dirs)))
    ))

;; menu only show modes according to the major-mode of the current buffer
(setq yas-use-menu 'abbreviate)
(yas-global-mode 1)
(diminish 'yas-minor-mode)

;; diff-hl ---------------------------------------------------------------------
(lambda-package-ensure-install 'diff-hl)
(require 'diff-hl)
(require 'diff-hl-dired)
(require 'diff-hl-flydiff)

(global-diff-hl-mode 1)
(diff-hl-flydiff-mode 1)
(add-hook 'dired-mode-hook 'diff-hl-dired-mode-unless-remote)

(add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

;; highlights parentheses, brackets, and braces according to their depth--------
(lambda-package-ensure-install 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode-enable)

(setq enable-local-eval t)
(setq enable-local-variables :all)
(setq enable-remote-dir-locals t)

;; M-? do not prompt, just use current word
(setq xref-prompt-for-identifier nil)

;; for comment-indent
(setq comment-fill-column 360)
(defun lambda-comment-indent (&rest args)
  "Apply CMD with ARGS to region lines if region is active.
Just call (apply CMD ARGS) otherwise."
  (interactive)
  (if (use-region-p)
      (cl-letf (((symbol-function 'execute-kbd-macro)
                 `(lambda (&rest _ignore)
                    (interactive)
                    (comment-indent ,@args))))
        (apply-macro-to-region-lines (region-beginning) (region-end) 'ignore))
    (comment-indent args)))
(global-set-key (kbd "M-;") 'lambda-comment-indent)

(defun lambda-tmp-buffer ()
  "Create temporary buffer."
  (interactive)
  (let ((buf-name (concat (make-temp-name "scratch-") "." (read-string (concat (propertize "File Format: " 'face '(bold default)))nil nil "json" nil))))
    (switch-to-buffer buf-name)
    (let ((buffer-file-name buf-name))
      (set-auto-mode))))

(lambda-package-ensure-install 'gxref)
(add-to-list 'xref-backend-functions 'gxref-xref-backend)

(use-package term/xterm
  :ensure nil ;; when use build-in package, add this to prenvent use-package load package from network
  :custom
  (xterm-max-cut-length 134217728))

(provide 'lambda-core)

;;; lambda-core.el ends here
