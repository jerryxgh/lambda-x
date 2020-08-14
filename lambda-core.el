;; lambda-core.el --- core settings, shared by all other modules

;; Time-stamp: <2019-06-07 23:19:41 Guanghui Xu>

;;; Commentary:
;; Core settings, shared by all other modules.

;;; Code:

;; Maxmize frame ---------------------------------------------------------------
;; fullscreen when startup finished
;; (custom-set-variables
;;  '(initial-frame-alist (quote ((fullscreen . fullboth)))))

(defun lambda-maxmize-frame ()
  "Make Emacs frame maxmized."
  (interactive)
  (cond ((and (eq system-type 'windows-nt)
              (fboundp 'w32-send-sys-command))
         (w32-send-sys-command 61488))
        ((eq system-type 'gnu/linux)
         (set-frame-parameter nil 'fullscreen 'maximized))
        (t
         (set-frame-parameter nil 'fullscreen 'maximized))))

(add-hook 'after-init-hook 'lambda-maxmize-frame)

(defconst current-user
  (getenv
   (if (equal system-type 'windows-nt) "USERNAME" "USER")))

(defconst lambda-x-direcotry (file-name-directory
                              (or load-file-name (buffer-file-name)))
  "Root directory of lambda-x.")

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
                                                       lambda-x-direcotry))

;; suppressing ad-handle-definition Warnings in Emacs
(setq ad-redefinition-action 'accept)

;; packages about settings =====================================================
(require 'package)

;; place package files relative to configuration directory
(setq package-user-dir (expand-file-name "packages/elpa" lambda-x-direcotry))

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

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
              ;;(featurep package)
              ;;(functionp package)
              )
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

;; init PATH in mac, this should just after packages settings ==================
(when (eq system-type 'darwin)
  ;; open file in Find will reuse current frame instead creating a new one

  (menu-bar-mode -1)
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


(when (and(eq system-type 'darwin) (= emacs-major-version 26))
  (add-to-list 'default-frame-alist '(ns-appearance . dark)))

;; theme -----------------------------------------------------------------------
;; (print (font-family-list))

(defun lambda-load-theme (theme)
  "Load THEME, plus that, set font and tweak mode-line style."
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
         (set-frame-font "menlo-13")
         (set-fontset-font "fontset-default" 'han '("PingFang SC"))
         (setq face-font-rescale-alist (list (cons "PingFang SC" 1.3)))
         )))

(lambda-package-ensure-install 'spacemacs-theme)
;; custom faces for spacemacs theme

(custom-set-faces
 ;; for auto-complete
 '(ac-gtags-candidate-face ((t (:inherit ac-candidate-face :bforeground "deep sky blue"))))
 '(ac-gtags-selection-face ((t (:inherit ac-selection-face :background "deep sky blue"))))
 ;; for woman
 '(woman-bold ((t (:inherit bold :foreground "#4f97d7"))))
 '(woman-italic ((t (:inherit italic :foreground "#c56ec3" :underline nil))))
 )
(lambda-load-theme 'spacemacs-dark)

;; (lambda-package-ensure-install 'zenburn-theme)
;; (lambda-load-theme 'zenburn)


(lambda-package-ensure-install 'powerline)
(lambda-package-ensure-install 'spaceline)

;;; window-numbering -----------------------------------------------------------
(defun window-numbering-install-mode-line (&optional position)
  "Do nothing, the display is handled by the spaceline(powerline).
POSITION: just inhibit warning.")

(lambda-package-ensure-install 'window-numbering)
(require 'window-numbering)
(setq window-numbering-auto-assign-0-to-minibuffer nil)
(window-numbering-mode 1)

(require 'spaceline-config)
;; see info: fonts
;; (set-face-font 'mode-line "-MS -Microsoft Yahei UI-normal-normal-normal-*-*-*-*-*-*-0-iso10646-1")
;; (set-face-font 'mode-line-inactive "-MS -Consolas-bold-normal-normal-*-*-*-*-*-*-0-iso10646-1")
;; (set-fontset-font "fontset-standard"
;;                   'unicode
;;                   (font-spec :family "Microsoft Yahei" :weight 'bold :size 10))
;; (set-face-font 'mode-line "fontset-standard")

(cond ((eq system-type 'windows-nt)
       (set-face-font 'mode-line "Microsoft Yahei-9:bold")
       (set-face-font 'mode-line-inactive "Microsoft Yahei-9:bold"))
      )
;; (set-face-font 'mode-line "Consolas-11:bold")
;; (set-face-font 'mode-line-inactive "Consolas-11:bold")

(setq spaceline-window-numbers-unicode t
      spaceline-workspace-numbers-unicode t
      powerline-default-separator nil
      ;; powerline-height 24 ;; make active and inactive modeline equal height
      spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
;; ;; fix the problem: ❻ is different from ➏
;; (defun spaceline--unicode-number (str)
;;   "Return a nice unicode representation of a single-digit number STR."
;;   (cond
;;    ((string= "1" str) "➊")
;;    ((string= "2" str) "➋")
;;    ((string= "3" str) "➌")
;;    ((string= "4" str) "➍")
;;    ((string= "5" str) "➎")
;;    ((string= "6" str) "➏")
;;    ((string= "7" str) "➐")
;;    ((string= "8" str) "➑")
;;    ((string= "9" str) "➒")
;;    ((string= "0" str) "➓")))

;;(lambda-package-ensure-install 'info+)
;;(require 'info+)
;; (spaceline-info-mode 1)
;; (spaceline-spacemacs-theme)
(spaceline-spacemacs-theme '(buffer-encoding process))
(redisplay)

;; inhibit annoying warning sound
(setq ring-bell-function 'ignore)

(mouse-avoidance-mode 'animate)
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
(setq sp-base-key-bindings 'paredit)
(setq sp-show-pair-from-inside t)
;; (setq sp-navigate-close-if-unbalanced t)
(require 'smartparens)
(require 'smartparens-config)
(setq sp-autoskip-closing-pair 'always)
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
      ((eq system-type 'darwin)
       (if (executable-find "gls")
           (progn
             (setq insert-directory-program (executable-find "gls"))
             (setq dired-listing-switches "-AlX --group-directories-first"))
         (setq dired-listing-switches "-Al")))
      (t (setq dired-listing-switches "-AlX --group-directories-first")))

;;; dired-subtree --------------------------------------------------------------
(lambda-package-ensure-install 'dired-subtree)
;; (unless (or (eq system-type 'windows-nt)
;;             (string-match-p "--dired" dired-listing-switches))
;;   (setq dired-listing-switches (concat dired-listing-switches " --dired")))
(define-key dired-mode-map (kbd "i") #'(lambda ()
                                         (interactive)
                                         (require 'dired-subtree)
                                         (if (dired-subtree--is-expanded-p)
                                             (message "already expanded")
                                           (dired-subtree-insert))))
(define-key dired-mode-map (kbd "K") 'dired-subtree-remove)
(define-key dired-mode-map (kbd "<tab>") 'dired-subtree-cycle)
(define-key dired-mode-map (kbd "C-i") 'dired-subtree-toggle)

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

(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

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
(setq anzu-cons-mode-line-p nil)
(global-anzu-mode)
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

;; whitespace-mode config.
;; (require 'whitespace)
;; (setq whitespace-line-column nil) ;; use fill-column instead of this
;; (setq whitespace-style '(face empty trailing lines-tail spaces newline
;;                               indentation space-after-tab space-before-tab
;;                               ;; big-indent
;;                               ))
;; (set 'whitespace-global-modes
;;      '(c++-mode c-mode conf-unix-mode emacs-lisp-mode haskell-mode lisp-mode lua-mode perl-mode python-mode scala-mode scheme-mode))
;; (global-whitespace-mode 0)
;; (diminish 'global-whitespace-mode)


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

;; (require 'sql)
;; (setq sql-mysql-options '("-C" "-t" "-f" "-n" "--default-character-set=utf8"))

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
;; (global-subword-mode 1)
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
(lambda-package-ensure-install 'flycheck)
(require 'flycheck)
(setq flycheck-emacs-lisp-initialize-packages t
      flycheck-emacs-lisp-package-user-dir package-user-dir
      flycheck-mode-line nil  ; use spaceline to show flycheck status instead
      )
;; enable on-the-fly syntax checking
(add-hook 'after-init-hook #'(lambda ()
                               (global-flycheck-mode)))
(lambda-package-ensure-install 'helm-flycheck)
(eval-after-load 'flycheck
  '(define-key flycheck-mode-map (kbd "C-c ! h") 'helm-flycheck))

;; sensible undo ---------------------------------------------------------------
(lambda-package-ensure-install 'undo-tree)
(require 'undo-tree)
;; autosave the undo-tree history
(setq undo-tree-history-directory-alist
      `((".*" . ,temporary-file-directory))
      undo-tree-auto-save-history t
      ;; undo-tree-visualizer-timestamps t
      )

(global-undo-tree-mode 1)
(diminish 'undo-tree-mode)
;;(add-to-list 'warning-suppress-types '(undo discard-info))

;;temp-buffer-browse -----------------------------------------------------------
(lambda-package-ensure-install 'temp-buffer-browse)
(require 'temp-buffer-browse)
(temp-buffer-browse-mode 1)

;; switch-window ---------------------------------------------------------------
;;(lambda-package-ensure-install 'switch-window)
;;(setq switch-window-shortcut-style 'qwerty)

;; shell configs ---------------------------------------------------------------
(require 'shell)
(require 'esh-mode)

(define-key shell-mode-map (kbd "C-j") 'comint-send-input)

(when (and (or
            (eq system-type 'gnu/linux)
            (eq system-type 'darwin))
           (file-exists-p "/bin/bash"))
  (setq explicit-shell-file-name "/bin/bash"))

;; close *compilation* buffer when compilation success
;; (add-hook 'compilation-start-hook 'kill-buffer-when-shell-command-exit)
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
              (define-key eshell-mode-map (kbd "C-j") 'eshell-send-input)))

;; do not load custom file, all the configuration should be done by code
(load "lambda-custom")

;;; ido --- interactively do things---------------------------------------------
;; ffap - find file at point is not userful when ido-mode is on
(lambda-package-ensure-install 'flx-ido)
(require 'flx-ido)

(require 'ido)
(setq ido-enable-flex-matching t
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
(ido-mode 1)
;;(ido-everywhere 1)
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
(lambda-package-ensure-install 'helm)
;; must set before helm-config,  otherwise helm use default
;; prefix "C-x c", which is inconvenient because you can
;; accidentially pressed "C-x C-c"
(setq-default helm-command-prefix-key "C-c h"
              ;; use ido-at-point
              helm-mode-handle-completion-in-region nil)
(require 'helm-config)
(helm-mode 1)

(define-key shell-mode-map (kbd "C-c C-l") 'helm-comint-input-ring)
(add-hook 'eshell-mode-hook
          #'(lambda ()
              (define-key eshell-mode-map (kbd "C-c C-l") 'helm-eshell-history)))
;; (setq magit-completing-read-function 'magit-ido-completing-read)
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
                (httpd-serve-directory . ido)
                (helm-gtags-create-tags . ido)
                (ffap-alternate-file . nil)
                (tmm-menubar . nil)))

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

;;; magit --- use git in emacs--------------------------------------------------
(lambda-package-ensure-install 'magit)
(require 'magit)
(require 'magit-ediff)
(setq magit-ediff-dwim-show-on-hunks t)
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
(global-diff-hl-mode +1)
(add-hook 'dired-mode-hook #'(lambda ()
                               (diff-hl-dired-mode 1)))

(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

;; unbound ---------------------------------------------------------------------
;;(lambda-package-ensure-install 'unbound)

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

(defun lambda-put-file-name-on-clipboard ()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))

(provide 'lambda-core)

;;; lambda-core.el ends here
