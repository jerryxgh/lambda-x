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

(defun lambda-copy-current-file-path-or-directory ()
  "Copy current buffer file'a path if exist, else try `default-directory`."
  (interactive)
  (cond
   ;; In buffers with file name
   ((buffer-file-name)
    (kill-new (buffer-file-name)))
   ((eq major-mode 'dired-mode)
    (kill-new (dired-current-directory)))
   ((kill-new default-directory))))

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
                                                       lambda-x-direcotry))

;; suppressing ad-handle-definition Warnings in Emacs
(setq ad-redefinition-action 'accept)

;; init PATH in mac, this should just after packages settings ==================
(when (eq system-type 'darwin)
  ;; open file in Find will reuse current frame instead creating a new one

  ;; close menu bar
  ;; (menu-bar-mode -1)
  (setq ns-pop-up-frames nil)
  (lambda-package-ensure-install 'exec-path-from-shell)
  (if (memq window-system '(mac ns))
      (exec-path-from-shell-initialize)))
;; init PATH in mac ends here ==================================================

;; Emacs UI about settings =====================================================

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; diminish keeps the modeline tidy, this package is needed by others, so put
;; this at the beginning -------------------------------------------------------
(lambda-package-ensure-install 'diminish)
(require 'diminish)

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


(when (and(eq system-type 'darwin) (> emacs-major-version 26))
  (add-to-list 'default-frame-alist '(ns-appearance . dark)))

;; theme -----------------------------------------------------------------------
(defun lambda-load-theme (theme)
  "Load THEME, plus that, set font and tweak mode-line style."
  ;; make font in the vertical middle of line
  (setq-default default-text-properties '(line-spacing 3 line-height 18))

  (load-theme theme t)

  (cond ((eq system-type 'windows-nt)
         (set-frame-font "Consolas-11")
         ;; (setq face-font-rescale-alist (list (cons "微软雅黑" 1.1)))
         (setq face-font-rescale-alist (list (cons "Î˘ČíŃĹşÚ" 1.1)))
         (if (fboundp 'set-fontset-font)
             (set-fontset-font t 'unicode '("Microsoft Yahei" .
                                            "unicode-bmp"))))
        ((eq system-type 'gnu/linux)
         (set-frame-font "Source Code Pro-11")
         (if (fboundp 'set-fontset-font)
             (set-fontset-font t 'unicode '("Noto Sans CJK SC" .
                                            "unicode-bmp")))

         (setq face-font-rescale-alist (list (cons "Noto Sans CJK SC" 1.2))))
        ((eq system-type 'darwin)
         (set-frame-font "menlo-14")
         (set-fontset-font "fontset-default" 'han '("PingFang SC"))
         (setq face-font-rescale-alist (list (cons "PingFang SC" 1.2)))
         )))

;; (lambda-package-ensure-install 'spacemacs-theme)
;; (lambda-load-theme 'spacemacs-dark)

;; (use-package solarized-theme
;;   :ensure
;;   :config
;;   (lambda-load-theme 'solarized-dark)
;;   )

(use-package zenburn-theme
  :ensure
  :config
  ;; use variable-pitch fonts for some headings and titles
  (setq zenburn-use-variable-pitch t)
  ;; scale headings in org-mode
  (setq zenburn-scale-org-headlines t)
  ;; scale headings in outline-mode
  (setq zenburn-scale-outline-headlines t)
  (lambda-load-theme 'zenburn))

;;; which-key --------------------------------------------------------------------
;; optional if you want which-key integration
(use-package which-key
  :ensure t
  :config
  ;; Allow C-h to trigger which-key before it is done automatically
  (setq which-key-show-early-on-C-h t)
  ;; make sure which-key doesn't show normally but refreshes quickly after it is
  ;; triggered.
  (setq which-key-idle-delay 10000)
  (setq which-key-idle-secondary-delay 0.05)
  (which-key-mode)
  (which-key-setup-side-window-bottom))

;;; mode line theme -------------------------------------------------------------
;; (lambda-package-ensure-install 'powerline)
(use-package powerline
  :ensure t
  :custom
  (powerline-height 23))

  ;; winum
(defun window-numbering-install-mode-line (&optional position)
  "Do nothing, the display is handled by the spaceline(powerline).
POSITION: just inhibit warning.")

(use-package winum
  :ensure t
  :config
  (winum-set-keymap-prefix (kbd "C-w"))
  (winum-mode))

(use-package spaceline
  :ensure t
  :config
  (cond ((eq system-type 'windows-nt)
         (set-face-font 'mode-line "Microsoft Yahei-9:bold")
         (set-face-font 'mode-line-inactive "Microsoft Yahei-9:bold")))

  (setq spaceline-window-numbers-unicode t
        spaceline-workspace-numbers-unicode t
        powerline-default-separator 'bar
        ;; different color for different evil state in modeline
        spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
  ;; (spaceline-spacemacs-theme '(buffer-encoding process))
  ;; this require is for spaceline-spacemacs-theme on the first installation of spaceline
  (require 'spaceline-config)
  (spaceline-spacemacs-theme)
  (spaceline-helm-mode)
  (redisplay))


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

;; auto insert newline at end of file if it has none
(setq-default require-final-newline t)
;; (setq case-fold-search nil)

;; directory to store all backup and autosave files
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; seperate system clipboard and emacs kill-ring
;; (setq select-enable-primary t)
;; (setq select-enable-clipboard t)
;; (setq select-active-regions nil)


;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode 1)
(diminish 'auto-revert-mode)
(electric-indent-mode 1)
;; using smartparens instead
;; (electric-pair-mode 1)

;; hippie expand is dabbrev expand on steroids
(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))
;; abbrev-mode settings
(setq abbrev-file-name (expand-file-name "abbrev_defs" lambda-auto-save-dir))
(setq-default abbrev-mode t)
(diminish 'abbrev-mode)
(if (file-exists-p abbrev-file-name)
    (quietly-read-abbrev-file))

;; smartparens -----------------------------------------------------------------
(lambda-package-ensure-install 'smartparens)
(require 'smartparens)
(require 'smartparens-config)
(setq sp-autoskip-closing-pair 'always)
;; use smartparens key bindings
(smartparens-global-mode t)
(smartparens-global-strict-mode t)
(show-smartparens-global-mode t)
(bind-keys
 :map smartparens-mode-map
 ;; ("C-M-a" . sp-beginning-of-sexp)
 ;; ("C-M-e" . sp-end-of-sexp)

 ;; ("C-<down>" . sp-down-sexp)
 ;; ("C-<up>"   . sp-up-sexp)
 ;; ("M-<down>" . sp-backward-down-sexp)
 ;; ("M-<up>"   . sp-backward-up-sexp)

 ;; ("C-M-f" . sp-forward-sexp)
 ;; ("C-M-b" . sp-backward-sexp)

 ;; ("C-M-n" . sp-next-sexp)
 ;; ("C-M-p" . sp-previous-sexp)

 ;; ("C-S-f" . sp-forward-symbol)
 ;; ("C-S-b" . sp-backward-symbol)

 ("C-<right>" . sp-forward-slurp-sexp)
 ("M-<right>" . sp-forward-barf-sexp)
 ("C-<left>"  . sp-backward-slurp-sexp)
 ("M-<left>"  . sp-backward-barf-sexp)

 ;; ("C-M-t" . sp-transpose-sexp)
 ;; ("C-M-k" . sp-kill-sexp)
 ;; ("C-k"   . sp-kill-hybrid-sexp)
 ;; ("M-k"   . sp-backward-kill-sexp)
 ;; ("C-M-w" . sp-copy-sexp)
 ;; ("C-M-d" . delete-sexp)

 ;; ("M-<backspace>" . backward-kill-word)
 ;; ("C-<backspace>" . sp-backward-kill-word)
 ;; ([remap sp-backward-kill-word] . backward-kill-word)

 ;; ("M-[" . sp-backward-unwrap-sexp)
 ;; ("M-]" . sp-unwrap-sexp)

 ;; ("C-x C-t" . sp-transpose-hybrid-sexp)

 ;; ("C-c ("  . wrap-with-parens)
 ;; ("C-c ["  . wrap-with-brackets)
 ;; ("C-c {"  . wrap-with-braces)
 ;; ("C-c '"  . wrap-with-single-quotes)
 ;; ("C-c \"" . wrap-with-double-quotes)
 ;; ("C-c _"  . wrap-with-underscores)
 ;; ("C-c `"  . wrap-with-back-quotes)
 )
(diminish 'smartparens-mode)

;; pair management
(sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)

;; markdown-mode
(sp-with-modes '(markdown-mode gfm-mode rst-mode)
  (sp-local-pair "*" "*" :bind "C-*")
  (sp-local-tag "2" "**" "**")
  (sp-local-tag "s" "```scheme" "```")
  (sp-local-tag "<"  "<_>" "</_>" :transform 'sp-match-sgml-tags))

;; tex-mode latex-mode
(sp-with-modes '(tex-mode plain-tex-mode latex-mode)
  (sp-local-tag "i" "\"<" "\">"))

;; html-mode
(sp-with-modes '(html-mode sgml-mode)
  (sp-local-pair "<" ">"))

;; lisp modes
(sp-with-modes sp--lisp-modes
  (sp-local-pair "(" nil :bind "C-("))

(with-eval-after-load 'smartparens
  (sp-with-modes
      '(c-mode c++-mode java-mode sh-mode css-mode go-mode)
    (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))))

;; uniquify --- easy to distinguish same name buffers
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

;; use shift + arrow keys to switch between visible buffers
(require 'windmove)
(windmove-default-keybindings)

;; highlight current line in prog-mode
(add-hook 'prog-mode-hook 'hl-line-mode)
(add-hook 'text-mode-hook 'hl-line-mode)

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
(setq ispell-personal-dictionary (expand-file-name "ispell" lambda-x-direcotry))

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
       (setq dired-listing-switches "-AlX"))
      ((eq system-type 'darwin)
       (if (executable-find "gls")
           (progn
             (setq insert-directory-program (executable-find "gls"))
             (setq dired-listing-switches "-AlX --group-directories-first"))
         (setq dired-listing-switches "-Al")))
      (t (setq dired-listing-switches "-AlX --group-directories-first")))

;;; dired-subtree --------------------------------------------------------------
;; (lambda-package-ensure-install 'dired-subtree)
;; (require 'dired-subtree)
;; (unless (or (eq system-type 'windows-nt)
;;             (string-match-p "--dired" dired-listing-switches))
;;   (setq dired-listing-switches (concat dired-listing-switches " --dired")))
;; (define-key dired-mode-map (kbd "i") #'(lambda ()
;;                                          (interactive)
;;                                          (if (dired-subtree--is-expanded-p)
;;                                              (message "already expanded")
;;                                            (dired-subtree-insert))))
;; (define-key dired-mode-map (kbd "K") 'dired-subtree-remove)
;; (define-key dired-mode-map (kbd "<tab>") 'dired-subtree-cycle)
;; (define-key dired-mode-map (kbd "C-i") 'dired-subtree-toggle)

;; if there is a dired buffer displayed in the next window, use its
;; current subdir, instead of the current subdir of this dired buffer
(setq dired-dwim-target t)


;;; bookmark -------------------------------------------------------------------
(require 'bookmark)
(setq bookmark-default-file (expand-file-name "bookmarks"
                                              lambda-auto-save-dir))

;; projectile is a project management mode -------------------------------------
(lambda-package-ensure-install 'projectile)
(require 'projectile)
(setq projectile-enable-caching t
      projectile-file-exists-remote-cache-expire nil
      projectile-completion-system 'helm
      ;; projectile-require-project-root nil
      projectile-cache-file (expand-file-name
                             "projectile.cache"
                             lambda-auto-save-dir)
      projectile-known-projects-file (expand-file-name
                                      "projectile-bookmarks.eld"
                                      lambda-auto-save-dir))

(define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map)

(projectile-mode t)
;;(diminish 'projectile-mode)

;; anzu-mode enhances isearch by showing total matches and current match
;; position --------------------------------------------------------------------
(lambda-package-ensure-install 'anzu)
(require 'anzu)
(setq anzu-cons-mode-line-p nil)
(global-anzu-mode 1)
(diminish 'anzu-mode)

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
(setq custom-file (expand-file-name "lambda-custom.el" lambda-x-direcotry)
      make-backup-files nil
      resize-mini-windows t
      enable-recursive-minibuffers t
      gc-cons-threshold 20480000
      confirm-kill-emacs 'y-or-n-p)


(if (string< emacs-version "24.3.50")
    (diminish 'global-visual-line-mode))
(diminish 'visual-line-mode)
;; enable to support navigate in camelCase words
(auto-compression-mode t)
(auto-image-file-mode t)

;; outline mode
(require 'outline)
(add-hook 'prog-mode-hook
          #'(lambda ()
              (outline-minor-mode t)))
(diminish 'outline-minor-mode)

;; enable hs-minor-mode for programming, hide or show code
(add-hook 'prog-mode-hook #'hs-minor-mode)

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

;; flycheck - much better than flymake -----------------------------------------
;; (lambda-package-ensure-install 'flycheck)
(use-package flycheck
  :ensure
  ;; enable on-the-fly syntax checking
  :init (global-flycheck-mode)
  :custom
  (compilation-skip-threshold 0)
  (flycheck-emacs-lisp-initialize-packages t)
  (flycheck-emacs-lisp-package-user-dir package-user-dir)
  ;; use spaceline to show flycheck status instead
  (flycheck-mode-line nil))

(use-package flycheck-package
  :ensure
  :config
  (flycheck-package-setup))

;; (lambda-package-ensure-install 'helm-flycheck)
(use-package helm-flycheck
  :ensure
  :config
  (define-key flycheck-mode-map (kbd "C-c ! h") 'helm-flycheck))

;; (use-package helm-flymake
;;   :ensure
;;   :config
;;   )

(use-package emacs
  :custom
  (switch-to-buffer-in-dedicated-window 'pop))

;; sensible undo ---------------------------------------------------------------
(lambda-package-ensure-install 'undo-tree)
(require 'undo-tree)
;; autosave the undo-tree history
(setq undo-tree-history-directory-alist
      `((".*" . ,temporary-file-directory))
      undo-tree-auto-save-history t
      ;; https://github.com/syl20bnr/spacemacs/issues/9903
      undo-tree-enable-undo-in-region nil
      ;; undo-tree-visualizer-timestamps t
      )

(global-undo-tree-mode 1)
(diminish 'undo-tree-mode)

;;temp-buffer-browse -----------------------------------------------------------
(lambda-package-ensure-install 'temp-buffer-browse)
(require 'temp-buffer-browse)
(temp-buffer-browse-mode 1)

;; shell configs ---------------------------------------------------------------
(require 'shell)
(require 'esh-mode)

(define-key shell-mode-map (kbd "C-j") 'comint-send-input)

;; (when (and (or
;;             (eq system-type 'gnu/linux)
;;             (eq system-type 'darwin))
;;            (file-exists-p "/bin/bash"))
;;   (setq explicit-shell-file-name "/bin/bash"))

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

;;; ido --- interactively do things---------------------------------------------
;; ffap - find file at point is not userful when ido-mode is on
(lambda-package-ensure-install 'flx-ido)
(require 'flx-ido)

(require 'ido)
(setq ido-enable-flex-matching t
      ido-use-url-at-point t
      ido-use-faces nil
      ido-auto-merge-work-directories-length -1
      ido-use-virtual-buffers t
      ido-ignore-buffers '("\\` "
                           "^\\*Ibuffer\\*$"
                           "^\\*helm.*\\*$"
                           "^\\*Compile-Log\\*$"
                           "^\\*Messages\\*$"
                           "^\\*Help\\*$")
      ido-save-directory-list-file
      (expand-file-name "ido.hist" lambda-auto-save-dir)
      ;; ido-cannot-complete-command 'ido-next-match
      ;; ido-default-file-method 'selected-window
      ;; ffap-require-prefix t ; get find-file-at-point with C-u C-x C-f
      )

(add-hook 'ido-setup-hook
          #'(lambda ()
              (define-key ido-completion-map (kbd "<tab>") 'ido-next-match)))

(setq ido-ignore-buffers  '("\\` " "^\\*.*\\*$"))
(put 'dired-do-copy   'ido nil) ; use ido there
(put 'dired-do-rename 'ido nil) ; ^
;; (put 'dired-do-rename 'ido 'find-file)
(ido-mode 1)
;;; smarter fuzzy matching for ido
(flx-ido-mode 1)

(lambda-package-ensure-install 'ido-completing-read+)
(require 'ido-completing-read+)
(ido-ubiquitous-mode 1)

;;; smex, remember recently and most frequently used commands ------------------
(lambda-package-ensure-install 'smex)
(require 'smex)
(setq smex-save-file (expand-file-name "smex-items"
                                       lambda-auto-save-dir))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;;; ido-at-point --- use ido to do completion-at-point -------------------------
(lambda-package-ensure-install 'ido-at-point)
(ido-at-point-mode 1)

;; helm ------------------------------------------------------------------------
;; (lambda-package-ensure-install 'helm)
(use-package helm
  :ensure
  :custom
  ;; always split window for helm
  (helm-split-window-inside-p t)
  (helm-move-to-line-cycle-in-source t)
  :config
  ;; must set before helm-config,  otherwise helm use default
  ;; prefix "C-x c", which is inconvenient because you can
  ;; accidentially pressed "C-x C-c"
  (setq-default helm-command-prefix-key "C-c h"
                ;; use ido-at-point
                helm-mode-handle-completion-in-region nil)
  (helm-mode 1)

  (define-key shell-mode-map (kbd "C-c C-l") 'helm-comint-input-ring)
  (add-hook 'eshell-mode-hook
            #'(lambda ()
                (define-key eshell-mode-map (kbd "C-c C-l") 'helm-eshell-history)))

  ;; to use with ido, customize helm-completing-read-handlers-alist
  (setq-default helm-completing-read-handlers-alist
                '((describe-function . ido)
                  (describe-variable . ido)
                  (where-is . ido)
                  (load-library . ido)
                  (debug-on-entry . ido)
                  (dired-do-copy . ido)
                  (dired-do-rename . ido)
                  (dired-create-directory . ido)
                  (find-function . ido)
                  (find-tag . ido)
                  (find-file . ido)
                  (find-file-other-window . ido)
                  (switch-to-buffer . ido)
                  (httpd-serve-directory . ido)
                  (ffap-alternate-file . nil)
                  (tmm-menubar . nil)))
  )

;; M-x helm-projectile-ag
(use-package helm-projectile
  :ensure t
  :custom
  (helm-ag-insert-at-point 'symbol)
  :config
  (helm-projectile-on)
  :pin melpa)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGE: helm-descbinds                      ;;
;;                                              ;;
;; GROUP: Convenience -> Helm -> Helm Descbinds ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(lambda-package-ensure-install 'helm-descbinds)
(require 'helm-descbinds)
(helm-descbinds-mode 1)
(lambda-package-ensure-install 'helm-ag)
(lambda-package-ensure-install 'ag)
(require 'ag)
(setq
 ;; If your version of ag is recent enough, you can add highlighting by
 ;; adding the following to your Emacs configuration:
 ag-highlight-search t
 ;; By default, ag.el will open results in a different window in the frame,
 ;; so the results buffer is still visible. You can override this so the
 ;; results buffer is hidden and the selected result is shown in its place:
 ag-reuse-window t
 )

;;; magit --- use git in emacs--------------------------------------------------
(use-package magit
  :ensure t
  ;; :bind ("C-c g" . magit-status)
  :custom
  ;; (magit-git-executable "/usr/local/bin/git")
  (magit-ediff-dwim-show-on-hunks t)
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
(global-set-key (kbd "C-x j") #'(lambda () (interactive)
                                  (ido-mode 1)
                                  (ido-find-file-in-dir lambda-x-direcotry)))

(setq mouse-wheel-scroll-amount '(3 ((shift) . 1)) ; three line at a time
      mouse-wheel-progressive-speed nil ; don't accelerate scrolling
      scroll-step 1
      scroll-preserve-screen-position t
      scroll-conservatively most-positive-fixnum)

;; YASnippet -------------------------------------------------------------------
(lambda-package-ensure-install 'yasnippet)
(lambda-package-ensure-install 'yasnippet-snippets)
(require 'yasnippet)
(add-to-list 'yas-snippet-dirs (expand-file-name "snippets" lambda-x-direcotry))
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

;; unicad --- say goodbye to Garbled -------------------------------------------
(require 'unicad)

;; highlights parentheses, brackets, and braces according to their depth--------
(lambda-package-ensure-install 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode-enable)

(setq enable-local-eval t)
(setq enable-local-variables :all)
(setq enable-remote-dir-locals t)

;; hungry-delete ---------------------------------------------------------------
(lambda-package-ensure-install 'hungry-delete)
(global-hungry-delete-mode 1)
(diminish 'hungry-delete-mode)

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
  (let ((buf-name (concat (make-temp-name "scratch-") "." (read-string "File Format: " nil nil "json" nil))))
    (switch-to-buffer buf-name)
    (let ((buffer-file-name buf-name))
      (set-auto-mode))))

(lambda-package-ensure-install 'gxref)
(add-to-list 'xref-backend-functions 'gxref-xref-backend)

(provide 'lambda-core)

;;; lambda-core.el ends here
