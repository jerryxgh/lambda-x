;; lambda-core.el --- core settings, shared by all other modules
;; Time-stamp: <2015-12-13 12:34:23 GuanghuiXu>

;;; Commentary:
;; Core settings, shared by all other modules.

;;; Code:

(defconst current-user
  (getenv
   (if (equal system-type 'windows-nt) "USERNAME" "USER")))

(defconst lambda-x-direcotry (file-name-directory
                              (or load-file-name (buffer-file-name)))
  "Root directory of lambda-x.")

(defconst lambda-auto-save-dir (expand-file-name "auto-save-list/"
                                                 user-emacs-directory)
  "This folder stores all the automatically generated save/history-files.")

;; (add-to-list 'load-path lambda-x-direcotry)
(add-to-list 'load-path (expand-file-name "packages/non-elpa" lambda-x-direcotry))
(add-to-list 'load-path "/home/xgh/repository/simplesite/")

;; suppressing ad-handle-definition Warnings in Emacs
(setq ad-redefinition-action 'accept)

;; packages about settings =====================================================
(require 'package)

;; add more package sources
(dolist (pkg-arch
         '(;;("marmalade" . "http://marmalade-repo.org/packages/")
           ;;("org" . "http://orgmode.org/elpa/")
           ;;("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")
           ("melpa" . "http://melpa.milkbox.net/packages/")
           ))
  (add-to-list 'package-archives pkg-arch nil))

;; place package files relative to configuration directory
(setq package-user-dir (expand-file-name "packages/elpa" lambda-x-direcotry))

;; do not auto load packages
(setq package-enable-at-startup nil)
;; Load packages explictly
(package-initialize)

(defvar lambda-package-installed-packages nil
  "Pakcages installed through `lambda-package-ensure-install'.

This value is set automaticly, DONT set by hand.")

(defun lambda-package-ensure-install (package)
  "This is like `package-install', but skip PACKAGE if it has been installed.

The difference is that if PACKAGE is already installed(checked through
 `package-installed-p'), it will not be installed again."
  (add-to-list 'lambda-package-installed-packages package)
  (unless (or (member package package-activated-list)
              (package-installed-p package)
              (featurep package)
              (functionp package))
    (message "Installing %s" (symbol-name package))
    (when (not package-archive-contents)
      (package-refresh-contents))
    (package-install package)))

(lambda-package-ensure-install 'dash)
(require 'dash)

(defun lambda-package-list-packages ()
  "Browse packages installed through function `lambda-package-ensure-install'."
  (interactive)
  (package-show-package-list lambda-package-installed-packages))

(defun lambda-package-list-auto-packages ()
  "Browse packages auto installed due to the dependence."
  (interactive)
  (package-show-package-list
   (-filter #'(lambda (p)
                (not (memq p lambda-package-installed-packages)))
            package-activated-list)))

(defun lambda-package-get-used-pkgs ()
  "Get all packages manually installed including requirements and \
requirements of requirements.
Which means get all used packages, this is mainly for getting unused packages."
  (delete-dups (-flatten
                (-map 'lambda-package-get-pkg-with-reqs
                      lambda-package-installed-packages))))

(defun lambda-package-get-pkg-with-reqs (package)
  "Get PACKAGE and requirements of PACKAGE and requirements of requirements."
  (if (and package
           (not (assq package package--builtins)))
      (cons package (-flatten
                     (-map #'(lambda (req)
                               (lambda-package-get-pkg-with-reqs (car req)))
                           (let ((pkg-desc
                                  (or (if (package-desc-p package) package)
                                      (cadr (assq package package-alist)))))
                             (if pkg-desc
                                 (package-desc-reqs pkg-desc))))))))

(defun lambda-package-list-unused-packages ()
  "Browse packages not used."
  (interactive)
  (package-show-package-list
   (let ((used-packages (lambda-package-get-used-pkgs)))
     (-filter #'(lambda (p)
                  (not (memq p used-packages)))
              package-activated-list))))

(lambda-package-ensure-install 'epl)
(require 'epl)

(defun lambda-package-update-packages ()
  "Update all packages installed by elpa."
  (interactive)
  (epl-upgrade)
  (message "Update finished. Restart Emacs to complete the process."))

;; packages about settings end here ============================================

;; Emacs UI about settings =====================================================

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; diminish keeps the modeline tidy, this package is needed by others, so put
;; this at the beginning -------------------------------------------------------
(lambda-package-ensure-install 'diminish)
(require 'diminish)

;; toolbar is just a waste of valuable screen estate in a tty tool-bar-mode does
;; not properly auto-load, and is already disabled anyway
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
(scroll-bar-mode -1)

;; (menu-bar-mode -1)

;; disable startup screen
(setq inhibit-startup-screen t)

;; nice scrolling in both keyboard and mouse
(setq mouse-wheel-scroll-amount '(3 ((shift) . 1)) ; three line at a time
      mouse-wheel-progressive-speed nil ; donnot accelerate scrolling
      scroll-step 1
      scroll-margin 0
      scroll-preserve-screen-position t
      scroll-conservatively most-positive-fixnum)

;; (setq window-resize-pixelwise t)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '("[" invocation-name " lambda-x] - "
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name)) "%b"))))

;; theme -----------------------------------------------------------------------
(lambda-package-ensure-install 'solarized-theme)
(require 'solarized)
(if (eq system-type 'gnu/linux)
    (setq x-underline-at-descent-line t))
;; Make the fringe stand out from the background.
(setq solarized-distinct-fringe-background t)
;; Use more italics.
(setq solarized-use-more-italic t)
(defun lambda-load-solarized-light-theme ()
  "Load solarized-light theme, plus that, set font and tweak mode-line style."
  (interactive)
  (lambda-load-theme 'solarized-light))

(defun lambda-load-solarized-dark-theme ()
  "Load solarized-dark theme, plus that, set font and tweak mode-line style."
  (interactive)
  (lambda-load-theme 'solarized-dark))

(defun lambda-load-theme (theme)
  "Load THEME, plus that, set font and tweak mode-line style."
  (load-theme theme t)
  ;; tweak mode line.
  (set-face-attribute 'mode-line nil :box nil)
  (set-face-attribute 'mode-line-inactive nil :box nil)

  (set-frame-font "Consolas-11")

  (if (eq system-type 'windows-nt)
      (setq face-font-rescale-alist (list (cons "Î˘ČíŃĹşÚ" 1.1)))
    (setq face-font-rescale-alist (list (cons "微软雅黑" 1.1))))

  (set-fontset-font t 'unicode '("Microsoft Yahei" .  "unicode-bmp")))

(lambda-load-solarized-dark-theme)

;; Emacs in OSX already has fullscreen support
;; Emacs has a similar built-in command in 24.4
;; Copied from prelude.
(defun lambda-ui-fullscreen ()
  "Make Emacs window fullscreen.

This follows freedesktop standards, should work in X servers."
  (interactive)
  (if (eq window-system 'x)
      (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                             '(2 "_NET_WM_STATE_FULLSCREEN" 0))
    (error "Only X server is supported")))

;; inhibit annoying warning sound
(setq ring-bell-function 'ignore)

(mouse-avoidance-mode 'animate)
(show-paren-mode 1)
;; (setq show-paren-style 'mixed)
;; (set-background-color "#CCE8CF")

(setq-default indicate-buffer-boundaries '((top . left) (t . right))
              indicate-empty-lines t)

;; use minibuffer instead of dialog to ask questions
(setq use-dialog-box nil)

;; Emacs UI about settings end here ===========================================

;; editor settings ============================================================

;; auto insert newline at end of file if it has none
(setq-default require-final-newline t)

;; directory to store all backup and autosave files
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode 1)

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
(setq abbrev-file-name (expand-file-name "abbrev_defs"
                                         lambda-auto-save-dir))
(setq-default abbrev-mode t)
(diminish 'abbrev-mode)
(if (file-exists-p abbrev-file-name)
    (quietly-read-abbrev-file))

;;; wirte a plugin to highlight TODO FIXME BUG KLUDGE on right fringe
(lambda-package-ensure-install 'fic-mode)
(defun annotate-todo ()
  "Put fringe marker on TODO: lines in the curent buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "TODO:" nil t)
      (let ((overlay (make-overlay (- (point) 5) (point))))
        (overlay-put overlay 'before-string
                     (propertize (format "A")
                                 'display
                                 '(right-fringe horizontal-bar)))))))

;; smartparens -----------------------------------------------------------------
(lambda-package-ensure-install 'smartparens)
;; this config should before (require 'smartparens)
(setq sp-base-key-bindings 'sp)
(setq sp-show-pair-from-inside t)
;; (setq sp-navigate-close-if-unbalanced t)
(require 'smartparens)
(require 'smartparens-config)
;; (setq sp-autoskip-closing-pair 'always)
(define-key smartparens-strict-mode-map
  [remap c-electric-backspace] 'sp-backward-delete-char)
;; use smartparens key bindings
(smartparens-global-mode t)
(smartparens-global-strict-mode t)
(show-smartparens-global-mode t)
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

(dolist (mode '(c-mode c++-mode java-mode sh-mode css-mode))
  (sp-local-pair mode
                 "{"
                 nil
                 :post-handlers
                 '((ome-create-newline-and-enter-sexp "RET"))))

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

;; volatile-highlights ---------------------------------------------------------
(lambda-package-ensure-install 'volatile-highlights)
(require 'volatile-highlights)
(volatile-highlights-mode t)
(diminish 'volatile-highlights-mode)

;;; tramp
;; usage: type `C-x C-f' and then enter the filename`/user@machine:/path/to.file
(add-to-list 'load-path (expand-file-name "packages/non-elpa/tramp/lisp"
                                          lambda-x-direcotry))
(add-to-list 'Info-default-directory-list
             (expand-file-name "packages/non-elpa/tramp/info"
                               lambda-x-direcotry))
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

;;; dired-x
(require 'dired-x)
(cond ((eq system-type 'windows-nt)
       (setq dired-listing-switches "-AlX"))
      (t (setq dired-listing-switches "-AlX  --group-directories-first")))

;; if there is a dired buffer displayed in the next window, use its
;; current subdir, instead of the current subdir of this dired buffer
(setq dired-dwim-target t)


;;; bookmark -------------------------------------------------------------------
(require 'bookmark)
(setq bookmark-default-file (expand-file-name "bookmarks"
                                              lambda-auto-save-dir))

;; ack A better grep for programmers -------------------------------------------
(lambda-package-ensure-install 'ack)
(lambda-package-ensure-install 'wgrep-ack)
(require 'ack)
(require 'wgrep-ack)
(setq ack-command (concat (file-name-nondirectory
                           (or (executable-find "ag")
                               (executable-find "ack")
                               (executable-find "ack-grep")
                               "ack")) " "))
;; C-c C-e : Apply the changes to file buffers.
;; C-c C-u : All changes are unmarked and ignored.
;; C-c C-d : Mark as delete to current line (including newline).
;; C-c C-r : Remove the changes in the region (these changes are not
;;           applied to the files. Of course, the remaining
;;           changes can still be applied to the files.)
;; C-c C-p : Toggle read-only area.
;; C-c C-k : Discard all changes and exit.
;; C-x C-q : Exit wgrep mode.

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
(projectile-global-mode t)
;;(diminish 'projectile-mode)
(defun projectile-ack (regexp &optional arg)
  "Run an ack search with REGEXP in the project.

With a prefix argument ARG prompts you for a directory on which
the search is performed ."
  (interactive
   (list (read-from-minibuffer
          (projectile-prepend-project-name "Ack search for: ")
          ;;(projectile-symbol-at-point)
          )
         current-prefix-arg))
  (if (require 'ack nil 'noerror)
      (let* ((root (if arg
                       (projectile-complete-dir)
                     (projectile-project-root))))
        (ack (concat ack-command regexp) root))
    (error "Ack not available")))

;; anzu-mode enhances isearch by showing total matches and current match
;; position --------------------------------------------------------------------
(lambda-package-ensure-install 'anzu)
(require 'anzu)
(global-anzu-mode)
(diminish 'anzu-mode)

;; ediff - don't start another frame
(require 'ediff)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(defvar ediff-saved-window-configuration nil
  "Window configuration before ediff.")

;;(add-hook 'ediff-before-setup-hook
;;          #'(lambda ()
;;              (setq ediff-saved-window-configuration
;;                    (current-window-configuration))))
;;(add-hook 'ediff-quit-hook
;;          #'(lambda ()
;;              (set-window-configuration
;;               ediff-saved-window-configuration))
;;          'append)
;;(add-hook 'ediff-suspend-hook
;;          #'(lambda ()
;;              (set-window-configuration
;;               ediff-saved-window-configuration))
;;          'append)

;; clean up obsolete buffers automatically
(require 'midnight)
(setq midnight-period 7200) ;; (eq (* 2 60 60) "2 hours")

;; whitespace-mode config.
(require 'whitespace)
(setq whitespace-line-column nil) ;; use fill-column instead of this
(setq whitespace-style '(face tabs empty trailing lines-tail spaces newline
                              indentation))
(set 'whitespace-global-modes
                        '(c-mode c++-mode java-mode emacs-lisp-mode scheme-mode
                                 lisp-mode python-mode lua-mode perl-mode
                                 haskell-mode scala-mode))
(global-whitespace-mode 1)


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
(setq user-full-name "GuanghuiXu"
      user-mail-address "gh_xu@qq.com"
      custom-file (expand-file-name "lambda-custom.el" lambda-x-direcotry)
      make-backup-files nil
      resize-mini-windows t
      enable-recursive-minibuffers t
      gc-cons-threshold 20480000
      source-directory "/home/xgh/sources/emacs-24.5"
      confirm-kill-emacs 'y-or-n-p)
(require 'sql)
(setq sql-mysql-options '("-C" "-t" "-f" "-n" "--default-character-set=utf8"))

;; Visual line mode is a new mode in Emacs 23. It provides support for editing
;; by visual lines. It turns on word-wrapping in the current buffer, and rebinds
;; C-a, C-e, and C-k to commands that operate by visual lines instead of logical
;; lines.  As you know, we have turn-on-auto-fill for text-mode and prog-mode
;; and all derived modes, which may make it useless to turn on visual-line-mode
;; most of the time. But we still turn on it globally to make it a fallback when
;; auto-fill-mode was disabled by users.
;;(global-visual-line-mode t)

(if (string< emacs-version "24.3.50")
    (diminish 'global-visual-line-mode))
(diminish 'visual-line-mode)
;; enable to support navigate in camelCase words
(global-subword-mode 1)
(auto-compression-mode t)
(auto-image-file-mode t)

;; outline mode
(require 'outline)
(add-hook 'prog-mode-hook
          #'(lambda ()
              (outline-minor-mode t)))
(diminish 'outline-minor-mode)

(defmacro with-region-or-buffer (func)
  "When called with no active region, call FUNC on current buffer."
  `(defadvice ,func (before with-region-or-buffer activate compile)
     (interactive
      (if mark-active
          (list (region-beginning) (region-end))
        (list (point-min) (point-max))))))

(with-region-or-buffer indent-region)
(with-region-or-buffer untabify)

;; diff-hl
(lambda-package-ensure-install 'diff-hl)
(global-diff-hl-mode +1)
(add-hook 'dired-mode-hook #'(lambda ()
                               (diff-hl-dired-mode 1)))

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
(setq epa-file-encrypt-to nil
      epa-file-cache-passphrase-for-symmetric-encryption t
      epa-file-inhibit-auto-save t)
(setenv "GPG_AGENT_INFO" nil) ; use minibuffer to input passphrase

;;; cal-china-x ----------------------------------------------------------------
(require 'cal-china-x)
(setq calendar-mark-holidays-flag t)
(setq cal-china-x-important-holidays cal-china-x-chinese-holidays)
(setq calendar-holidays cal-china-x-important-holidays)

;; flycheck - much better than flymake -----------------------------------------
(lambda-package-ensure-install 'flycheck)
(require 'flycheck)
(setq flycheck-emacs-lisp-initialize-packages t)
(setq flycheck-emacs-lisp-package-user-dir package-user-dir)
;; enable on-the-fly syntax checking
(if (fboundp 'global-flycheck-mode)
    (global-flycheck-mode 1)
  (add-hook 'prog-mode-hook 'flycheck-mode))
(lambda-package-ensure-install 'helm-flycheck)
(eval-after-load 'flycheck
  '(define-key flycheck-mode-map (kbd "C-c ! h") 'helm-flycheck))

;; sensible undo ---------------------------------------------------------------
(lambda-package-ensure-install 'undo-tree)
(require 'undo-tree)
;; autosave the undo-tree history
(setq undo-tree-history-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq undo-tree-auto-save-history t)

(global-undo-tree-mode 1)
(diminish 'undo-tree-mode)
;;(add-to-list 'warning-suppress-types '(undo discard-info))

;;(require 'temp-buffer-browse) ------------------------------------------------
(lambda-package-ensure-install 'temp-buffer-browse)
(require 'temp-buffer-browse)
(temp-buffer-browse-mode 1)

;; switch-window ---------------------------------------------------------------
;;(lambda-package-ensure-install 'switch-window)
;;(setq switch-window-shortcut-style 'qwerty)

;; shell configs ---------------------------------------------------------------
(require 'shell)
(require 'esh-mode)
(defun clear ()
  "Clear `eshell' or submode of `comint-mode' buffer."
  (interactive)
  (cond ((eq major-mode 'eshell-mode)
         (let ((eshell-buffer-maximum-lines 0))
           (eshell-truncate-buffer)))
        ((derived-mode-p 'comint-mode)
         (let ((comint-buffer-maximum-size 0))
           (comint-truncate-buffer)))))
(define-key shell-mode-map (kbd "C-j") 'comint-send-input)
(define-key shell-mode-map (kbd "C-l") 'clear)

(when (and (eq system-type 'gnu/linux)
           (file-exists-p "/bin/bash"))
  (setq explicit-shell-file-name "/bin/bash"))

(defun kill-buffer-when-shell-command-exit (&optional buffer)
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
               (quit-window t (get-buffer-window (process-buffer process)))))))))

;; close *compilation* buffer when compilation success
;; (add-hook 'compilation-start-hook 'kill-buffer-when-shell-command-exit)
(add-hook 'comint-mode-hook 'kill-buffer-when-shell-command-exit)
;; always insert at the bottom of shell like buffers
(setq comint-scroll-to-bottom-on-input t)
;; (setq shell-cd-regexp "")
;; no duplicates in command history
(setq comint-input-ignoredups t)

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
              (define-key eshell-mode-map (kbd "C-j") 'eshell-send-input)
              (define-key eshell-mode-map (kbd "C-l") 'clear)))

;; do not load custom file, all the configuration should be done by code
;; (load "lambda-custom")

;;; ido --- interactively do things---------------------------------------------
;; ffap - find file at point is not userful when ido-mode is on
(lambda-package-ensure-install 'ido-ubiquitous)
(lambda-package-ensure-install 'flx-ido)
(require 'ido)
(require 'ido-ubiquitous)
(require 'flx-ido)
(setq ido-enable-flex-matching t
      ido-auto-merge-work-directories-length -1
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
              (define-key ido-completion-map (kbd "<tab>") 'ido-next-match)
              ))

(defun lambda-ido-find-file-at-point ()
  "Use ffap as wanted."
  (interactive)
  (let ((ido-use-filename-at-point 'guess)
        (ido-use-url-at-point 'guess))
    (ido-find-file)))

(setq ido-ignore-buffers  '("\\` " "^\\*.*\\*$"))
(put 'dired-do-copy   'ido nil) ; use ido there
(put 'dired-do-rename 'ido nil) ; ^
;; (put 'dired-do-rename 'ido 'find-file)
(ido-mode t)
(ido-everywhere t)
(ido-ubiquitous-mode 1)
;;; smarter fuzzy matching for ido
(flx-ido-mode 1)

;;; smex, remember recently and most frequently used commands ------------------
(lambda-package-ensure-install 'smex)
(require 'smex)
(setq smex-save-file (expand-file-name "smex-items"
                                       lambda-auto-save-dir))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; helm ------------------------------------------------------------------------
(lambda-package-ensure-install 'helm)

(require 'helm)
;; must set before helm-config,  otherwise helm use default
;; prefix "C-x c", which is inconvenient because you can
;; accidentially pressed "C-x C-c"
(setq-default helm-command-prefix-key "C-c h")
(require 'helm-config)
(require 'helm-files)
(require 'helm-grep)

(global-set-key (kbd "M-y") 'helm-show-kill-ring)

;; rebihnd tab to do persistent action
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
;; make TAB works in terminal
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
;; list actions using C-z
(define-key helm-map (kbd "C-z")  'helm-select-action)
(define-key shell-mode-map (kbd "C-c C-l") 'helm-comint-input-ring)
(require 'helm-eshell)
(add-hook 'eshell-mode-hook
          #'(lambda ()
              (define-key eshell-mode-map (kbd "C-c C-l") 'helm-eshell-history)))

(define-key helm-grep-mode-map (kbd "<return>")
  'helm-grep-mode-jump-other-window)
(define-key helm-grep-mode-map (kbd "n")
  'helm-grep-mode-jump-other-window-forward)
(define-key helm-grep-mode-map (kbd "p")
  'helm-grep-mode-jump-other-window-backward)
(setq helm-net-prefer-curl t
      helm-scroll-amount 4 ; scroll 4 lines other window using M-<next>/M-<prior>
      helm-quick-update t ; do not display invisible candidates
      ;; be idle for this many seconds, before updating in delayed sources.
      helm-idle-delay 0.01
      ;; be idle for this many seconds, before updating candidate buffer
      helm-input-idle-delay 0.01
      ;; search for library in `require' and `declare-function' sexp.
      helm-ff-search-library-in-sexp t

      ;; open helm buffer in another window
      helm-split-window-default-side 'other
      ;; open helm buffer inside current window, not occupy whole other window
      helm-split-window-in-side-p t
      helm-buffers-favorite-modes (append helm-buffers-favorite-modes
                                          '(picture-mode artist-mode))
      ;; limit the number of displayed canidates
      helm-candidate-number-limit 200
      helm-boring-file-regexp-list
      ;; do not show these files in helm buffer
      '("\\.git$" "\\.hg$" "\\.svn$" "\\.CVS$" "\\._darcs$" "\\.la$" "\\.o$"
        "\\.i$")
      helm-ff-file-name-history-use-recentf t
      helm-move-to-line-cycle-in-source t ; move to end or beginning of source
      ;; when reaching top or bottom of source.
      ido-use-virtual-buffers t      ; Needed in helm-buffers-list
      ;; fuzzy matching buffer names when non-nil useful in helm-mini that lists
      ;; buffers
      helm-buffers-fuzzy-matching t)

;; save current position to mark ring when jumping to a different place
(add-hook 'helm-goto-line-before-hook 'helm-save-current-pos-to-mark-ring)

;; (require 'helm-mode)
;; to use with ido, customize helm-completing-read-handlers-alist
(setq-default helm-completing-read-handlers-alist
              '((describe-function . ido)
                (describe-variable . ido)
                (load-library . ido)
                (debug-on-entry . ido)
                (dired-do-copy . ido)
                (find-function . ido)
                (find-tag . ido)
                (ffap-alternate-file . nil)
                (tmm-menubar . nil)))
(helm-mode 1)
(diminish 'helm-mode)

;; helm-ls-git Yet another helm for listing the files in a git repo. -----------
;;(lambda-package-ensure-install 'helm-ls-git)
;;(require 'helm-ls-git)
;;(global-set-key (kbd "C-<f6>") 'helm-ls-git-ls)
;;(global-set-key (kbd "C-x C-d") 'helm-browse-project)

(lambda-package-ensure-install 'helm-projectile)
(require 'helm-projectile)
(helm-projectile-on)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGE: helm-descbinds                      ;;
;;                                              ;;
;; GROUP: Convenience -> Helm -> Helm Descbinds ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(lambda-package-ensure-install 'helm-descbinds)
(require 'helm-descbinds)
(helm-descbinds-mode 1)
(lambda-package-ensure-install 'helm-ag)

;;; ido-at-point --- use ido to do completion-at-point -------------------------
(lambda-package-ensure-install 'ido-at-point)
(require 'ido-at-point) ; unless installed from a package
(ido-at-point-mode 1)


;;; magit --- use git in emacs--------------------------------------------------
(lambda-package-ensure-install 'magit)
(require 'magit)
;; (magit-define-popup-action 'magit-ediff-popup ?S "Show staged"
;;  'magit-ediff-show-staged)
;; (magit-define-popup-action 'magit-ediff-popup ?U "Show unstaged"
;;  'magit-ediff-show-unstaged)
;; windows support
(let ((git-executable-windows "C:/Program Files (x86)/Git/bin/git.exe"))
  (when (and (eq system-type 'windows-nt)
             (file-exists-p git-executable-windows))
    (setq magit-git-executable git-executable-windows)
    (setenv "PATH"
            (concat (getenv "PATH") ";c:/Program Files (x86)/Git/bin/"))))
;; (setq magit-last-seen-setup-instructions "1.4.0")
;; magit-ediff-restore

;; (setq exec-path (append exec-path '("c:/Program Files (x86)/Git/bin/")))

;;; fill-column ----------------------------------------------------------------
(setq-default fill-column 80)
(lambda-package-ensure-install 'fill-column-indicator)
(add-hook 'prog-mode-hook #'(lambda ()
                              (turn-on-auto-fill)
                              ;; (turn-on-fci-mode)
                              ))
;; mode names typically end in "-mode", but for historical reasons
;; auto-fill-mode is named by "auto-fill-function".
(diminish 'auto-fill-function)

;; key-bindings for myself, you can change this to yours -----------------------
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "C-\\") 'toggle-truncate-lines)
(global-set-key (kbd "C-<") 'shrink-window)
(global-set-key (kbd "C->") 'enlarge-window)
(global-set-key (kbd "M-n") 'next-error)
(global-set-key (kbd "M-p") 'previous-error)
(define-key global-map (kbd "C-x C-z") 'goto-previous-buffer)
(global-set-key (kbd "C-x j") #'(lambda () (interactive)
                                  (ido-find-file-in-dir lambda-x-direcotry)))

(setq mouse-wheel-scroll-amount '(3 ((shift) . 1)) ; three line at a time
      mouse-wheel-progressive-speed nil ; don't accelerate scrolling
      scroll-step 1
      scroll-preserve-screen-position t
      scroll-conservatively most-positive-fixnum)

;; YASnippet -------------------------------------------------------------------
(lambda-package-ensure-install 'yasnippet)
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
(setq yas-dont-activate #'(lambda ()
                            (or (minibufferp)
                                buffer-read-only)))

;; auto-complete ---------------------------------------------------------------
(lambda-package-ensure-install 'auto-complete)
(global-auto-complete-mode 1)
(diminish 'auto-complete-mode)
(require 'auto-complete-config)
(ac-config-default)
(add-to-list 'ac-dictionary-directories
             (expand-file-name "ac-dict" lambda-x-direcotry))
(add-to-list 'ac-dictionary-files
             (expand-file-name "ac-dict/auto-complete.dict" lambda-x-direcotry))
(setq ac-comphist-file (expand-file-name "ac-comphist.dat"
                                         lambda-auto-save-dir)
      ;; ac-auto-start 3
      ac-ignore-case nil
      ac-modes
      (append ac-modes
              '(eshell-mode shell-mode graphviz-dot-mode
                            conf-xdefaults-mode html-mode nxml-mode objc-mode
                            sql-mode change-log-mode text-mode
                            makefile-gmake-mode makefile-bsdmake-mo
                            autoconf-mode makefile-automake-mode snippet-mode
                            cmake-mode octave-mode conf-javaprop-mode
                            nginx-mode))
      ac-use-menu-map t)

(setq-default ac-sources (append '(ac-source-filename
                                   ;; ac-source-yasnippet
                                   )
                                 ac-sources))

(define-key ac-mode-map (kbd "M-/") 'auto-complete)
(define-key ac-completing-map (kbd "<tab>") 'ac-expand)
(define-key ac-completing-map (kbd "<backtab>") 'ac-previous)
(define-key ac-completing-map (kbd "<return>") 'ac-complete)

;; pos-tip ---------------------------------------------------------------------
(lambda-package-ensure-install 'pos-tip)
(require 'pos-tip)
(setq ac-quick-help-prefer-pos-tip t)

;;popup ------------------------------------------------------------------------
(lambda-package-ensure-install 'popup)
(require 'popup)
;; add some shotcuts in popup menu mode
(define-key popup-menu-keymap (kbd "M-n") 'popup-next)
(define-key popup-menu-keymap (kbd "<tab>") 'popup-next)
(define-key popup-menu-keymap (kbd "<backtab>") 'popup-previous)
(define-key popup-menu-keymap (kbd "M-p") 'popup-previous)
(define-key popup-menu-keymap (kbd "<escape>")
  #'(lambda nil
      (interactive)
      (if (featurep 'evil)
          (evil-force-normal-state))
      (keyboard-quit)))

(defun yas-popup-isearch-prompt (prompt choices &optional display-fn)
  "Use popup to prompt PROMPT and CHOICES.
DISPLAY-FN: use this function to display."
  (when (featurep 'popup)
    (popup-menu*
     (mapcar
      #'(lambda (choice)
          (popup-make-item
           (or (and display-fn (funcall display-fn choice))
               choice)
           :value choice))
      choices)
     :prompt prompt
     ;; start isearch mode immediately
     ;;:isearch t
     )))

(add-to-list 'yas-prompt-functions 'yas-popup-isearch-prompt)

(yas-global-mode 1)
(diminish 'yas-minor-mode)

;; unbound ---------------------------------------------------------------------
(lambda-package-ensure-install 'unbound)

;; unicad --- say goodbye to Garbled -------------------------------------------
(require 'unicad)

;; highlights parentheses, brackets, and braces according to their depth--------
(lambda-package-ensure-install 'rainbow-delimiters)
;; global-rainbow-delimiters-mode will bring errors
(if (> emacs-major-version 23)
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'c-mode-common-hook 'rainbow-delimiters-mode)
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'python-mode-hook 'rainbow-delimiters-mode))

(setq enable-local-eval t)
(setq enable-local-variables :all)
(setq enable-remote-dir-locals t)

(provide 'lambda-core)

;;; lambda-core.el ends here
