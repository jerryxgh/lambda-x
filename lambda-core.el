;; lambda-core.el --- core settings, shared by most other modules
;; Time-stamp: <2014-04-21 18:44:13 Jerry Xu>
;;; Commentary:
;; core settings

;;; Code:

;; diminish keeps the modeline tidy --------------------------------------------
(lambda-package-ensure-install 'diminish)
(require 'diminish)

;; Miscellaneous basic settings ------------------------------------------------
(setq user-full-name "Jerry Xu"
      user-mail-address "jerryxgh@gmail.com"
      inhibit-startup-screen t 
      abbrev-file-name (expand-file-name "auto-save-list/abbrev_defs"
										 user-emacs-directory)

      custom-file (concat lambda-x-direcotry "lambda-custom.el")
      use-dialog-box nil 
      sql-mysql-options '("-C" "-t" "-f" "-n" "--default-character-set=utf8") 
      make-backup-files nil 
      resize-mini-windows t 
      ring-bell-function 'ignore ; inhibit annoying warning sound
      x-select-enable-clipboard t 
      enable-recursive-minibuffers t 
      ;;confirm-kill-emacs 'y-or-n-p
	  )
;; abbrev-mode -----------------------------------------------------------------
(setq-default abbrev-mode t)
(diminish 'abbrev-mode)
(if (file-exists-p abbrev-file-name)
    (quietly-read-abbrev-file))

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
;; set text-mode as the default major mode, instead of fundamental-mode
(setq-default major-mode 'text-mode)
;; let one line display as one line, even if it over the window
(setq-default truncate-lines t)


;; if there is a dired buffer displayed in the next window, use its
;; current subdir, instead of the current subdir of this dired buffer
(setq dired-dwim-target t)

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

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '("[" invocation-name " lambda-x] - "
		(:eval (if (buffer-file-name)
				   (abbreviate-file-name (buffer-file-name)) "%b"))))
(setq-default tab-width 4)
;;(add-to-list 'Info-default-directory-list " ")
(cond ((eq system-type 'windows-nt)
       (setq dired-listing-switches "-AlX"))
      (t (setq dired-listing-switches "-AlX  --group-directories-first")))

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
;; Visual line mode is a new mode in Emacs 23. It provides support for editing
;; by visual lines. It turns on word-wrapping in the current buffer, and rebinds
;; C-a, C-e, and C-k to commands that operate by visual lines instead of logical
;; lines.  As you know, we have turn-on-auto-fill for text-mode and prog-mode
;; and all derived modes, which may make it useless to turn on visual-line-mode
;; most of the time. But we still turn on it globally to make it a fallback when
;; auto-fill-mode was disabled by users.
(global-visual-line-mode t)
(if (string< emacs-version "24.3.50")
	(diminish 'global-visual-line-mode))
(diminish 'visual-line-mode)
(global-auto-revert-mode 1) 
;; enable to support navigate in camelCase words
(global-subword-mode 1)
(auto-compression-mode t)
(auto-image-file-mode t)
;; appearance
(column-number-mode t)
(line-number-mode t)
(size-indication-mode t)

;; enable winner-mode to manage window configurations
(winner-mode 1)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

(mouse-avoidance-mode 'animate) 
(show-paren-mode 1)
(setq show-paren-style 'mixed)
(tooltip-mode 0) 
;; highlight current line
(global-hl-line-mode 1)
(scroll-bar-mode 0) 
;; the toolbar is just a waste of valuable screen estate
;; in a tty tool-bar-mode does not properly auto-load, and is
;; already disabled anyway
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(set-frame-font "Consolas-11")
;;(set-background-color "#CCE8CF") 
;;(set-fontset-font t 'unicode (font-spec :family "Microsoft Yahei" 
;;									:registry "unicode-bmp"
;;										:size 1.1))
(setq scalable-fonts-allowed t)
;; Align chinese font in org table, solution is from below:
;; http://baohaojun.github.io/blog/2012/12/19/perfect-emacs-chinese-font.html
(if (eq system-type 'windows-nt)
	(setq face-font-rescale-alist (list (cons "Î˘ČíŃĹşÚ" 1.1)))
  (setq face-font-rescale-alist (list (cons "微软雅黑" 1.1))))

(set-fontset-font t 'unicode '("Microsoft Yahei" .  "unicode-bmp"))
(setq-default indicate-buffer-boundaries '((top . left) (t . right))
              indicate-empty-lines t)

;; outline mode ----------------------------------------------------------------
(require 'outline)
(add-hook 'prog-mode-hook
          (lambda ()
            (outline-minor-mode t)))
(diminish 'outline-minor-mode)

;; flyspell --------------------------------------------------------------------
(if (eq system-type 'windows-nt)
	(add-to-list 'exec-path "C:/Program Files (x86)/Aspell/bin/"))
(setq ispell-program-name "aspell")
(setq ispell-personal-dictionary (expand-file-name "ispell" lambda-x-direcotry))

;; flycheck --------------------------------------------------------------------
(lambda-package-ensure-install 'flycheck)
;; enable on-the-fly syntax checking
(if (fboundp 'global-flycheck-mode)
    (global-flycheck-mode 1)
  (add-hook 'prog-mode-hook 'flycheck-mode))

;; whitespace-mode config ------------------------------------------------------
(require 'whitespace)
(setq whitespace-line-column 80) ;; limit line length
(setq whitespace-style '(face tabs empty trailing lines-tail))

;; saner regex syntax ----------------------------------------------------------
(require 're-builder)
(setq reb-re-syntax 'string)

;; ediff - don't start another frame -------------------------------------------
(require 'ediff)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; use shift + arrow keys to switch between visible buffers --------------------
(require 'windmove)
(windmove-default-keybindings)

;; uniquify --- easy to distinguish same name buffers---------------------------
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

;; theme -----------------------------------------------------------------------
(lambda-package-ensure-install 'solarized-theme)
;; make the fringe stand out from the background
(setq solarized-distinct-fringe-background t)
;; make the modeline high contrast
;;(setq solarized-high-contrast-mode-line t)
;; Use less bolding
;;(setq solarized-use-less-bold t)
;; Use more italics
(setq solarized-use-more-italic t)
;; Use less colors for indicators such as git:gutter, flycheck and similar.
;;(setq solarized-emphasize-indicators nil)
(load-theme 'solarized-dark t)
(set-face-attribute 'mode-line nil :box nil)
(set-face-attribute 'mode-line nil :underline nil)
(set-face-attribute 'mode-line-highlight nil :box nil)
(set-face-attribute 'mode-line-highlight nil :underline nil)
(set-face-attribute 'mode-line-inactive nil :box nil)
(set-face-attribute 'mode-line-inactive nil :underline nil)

;; powerline -------------------------------------------------------------------
(lambda-package-ensure-install 'powerline)
(powerline-default-theme)

;; sensible undo ---------------------------------------------------------------
(lambda-package-ensure-install 'undo-tree)
(global-undo-tree-mode 1)

;; volatile-highlights ---------------------------------------------------------
(lambda-package-ensure-install 'volatile-highlights)
(require 'volatile-highlights)
(volatile-highlights-mode t)
(diminish 'volatile-highlights-mode)

;;(require 'temp-buffer-browse) ------------------------------------------------
(lambda-package-ensure-install 'temp-buffer-browse)
(temp-buffer-browse-mode 1)

;;switch-window ----------------------------------------------------------------
;;(lambda-package-ensure-install 'switch-window)
;;(setq switch-window-shortcut-style 'qwerty)

(defun create-scratch-buffer nil
  "Create a scratch buffer with the scratch message."
  (interactive)
  (unless (get-buffer "*scratch*")
    (switch-to-buffer (get-buffer-create "*scratch*"))
    (insert initial-scratch-message)
    (goto-char (point-max))
    (lisp-interaction-mode)))

(defun clear-scratch-buffer nil 
  "Clear the scratch buffer and keep the scratch message."
  (interactive)
  (when (equal (buffer-name (current-buffer)) "*scratch*")
    (delete-region (point-min) (point-max))
    (insert initial-scratch-message)
    (goto-char (point-max))))

(defun interactive-shell-on-exit-kill-buffer nil
  "Kill the buffer on exit of interactive shell."
  (let ((process (get-buffer-process (current-buffer))))
    (if process
        (set-process-sentinel 
		 process
		 (lambda (process state)
		   (when (or (string-match "exited abnormally with code." state)
					 (string-match "finished" state))
			 (kill-buffer (current-buffer))))))))

;; Do not load custom file, all the configuration should be done by code
;;(load "lambda-custom") 

(add-hook 'comint-mode-hook 'interactive-shell-on-exit-kill-buffer)
;; always insert at the bottom
(setq comint-scroll-to-bottom-on-input t)
;; no duplicates in command history
(setq comint-input-ignoredups t)

;;; ido --- interactively do things---------------------------------------------
(lambda-package-ensure-install 'ido-ubiquitous)
(lambda-package-ensure-install 'flx-ido)
(require 'ido)
(require 'ido-ubiquitous)
(require 'flx-ido)
(setq ido-enable-flex-matching t
      ido-auto-merge-work-directories-length -1
      ido-save-directory-list-file
	  (expand-file-name "auto-save-list/ido.hist" user-emacs-directory)
      ido-default-file-method 'selected-window)
;;(setq ido-ignore-buffers  '("\\` " "^\\*.*\\*$"))
(ido-mode t)
(ido-everywhere t)
(ido-ubiquitous-mode 1)
;;; smarter fuzzy matching for ido
(flx-ido-mode 1)

;;; smex, remember recently and most frequently used commands ------------------
(lambda-package-ensure-install 'smex)
(require 'smex)
(setq smex-save-file (expand-file-name "auto-save-list/smex-items"
									   user-emacs-directory))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; helm ------------------------------------------------------------------------
(lambda-package-ensure-install 'helm)
(lambda-package-ensure-install 'helm-projectile)
(require 'helm-config)
(helm-mode 1)
(diminish 'helm-mode)
(require 'helm-projectile)

;;; tramp ----------------------------------------------------------------------
;; Usage: type `C-x C-f' and then enter the filename`/user@machine:/path/to.file
(require 'tramp)
(when (> emacs-major-version 23)
  (require 'tramp-sh)
  (delete "LC_ALL=C" tramp-remote-process-environment)
  (add-to-list 'tramp-remote-process-environment "LANG=zh_CN.utf8" 'append)
  (add-to-list 'tramp-remote-process-environment "LC_ALL=zh_CN.utf8" 'append))

(setq tramp-auto-save-directory  (expand-file-name "auto-save-list/tramp"
												   user-emacs-directory)

      ido-enable-tramp-completion t)
(if (eq system-type 'windows-nt)
    (setq tramp-default-method "plink")
  (setq tramp-default-method "ssh"))

;;; ibuffer --------------------------------------------------------------------
(global-set-key (kbd "C-x C-b") 'ibuffer) 
;;(setq ibuffer-never-show-predicates (list "^ ?\\*.*\\*$"))

;;; time-stamp -----------------------------------------------------------------
(setq time-stamp-active t
      time-stamp-warn-inactive t
      time-stamp-format "%:y-%02m-%02d %02H:%02M:%02S %U")
(add-hook 'before-save-hook 'time-stamp)

;;; cal-china-x ----------------------------------------------------------------
(require 'cal-china-x)
(setq calendar-mark-holidays-flag t)
(setq cal-china-x-important-holidays cal-china-x-chinese-holidays)
(setq calendar-holidays cal-china-x-important-holidays)

;;; easypg ---------------------------------------------------------------------
;;(require 'epa) 
(setq epa-file-encrypt-to nil
      epa-file-cache-passphrase-for-symmetric-encryption t 
      epa-file-inhibit-auto-save t) 
(setenv "GPG_AGENT_INFO" nil) ; use minibuffer to input passphrase

;;; magit --- use git in emacs--------------------------------------------------
;;(require 'magit)
(lambda-package-ensure-install 'magit)
(let ((git-executable-windows "C:/Program Files (x86)/Git/bin/git.exe"))
  (when (and (eq system-type 'windows-nt)
             (file-exists-p git-executable-windows))
    (setq magit-git-executable git-executable-windows)
    (setenv "PATH"
            (concat (getenv "PATH") ";c:/Program Files (x86)/Git/bin/"))))

;; (setq exec-path (append exec-path '("c:/Program Files (x86)/Git/bin/")))

;;; dired-x --------------------------------------------------------------------
(require 'dired-x)

;;; fill-column ----------------------------------------------------------------
(setq-default fill-column 80)
(lambda-package-ensure-install 'fill-column-indicator)
;;(require 'fill-column-indicator)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'prog-mode-hook '(lambda ()
							 (turn-on-auto-fill)
							 (turn-on-fci-mode)))
;; Mode names typically end in "-mode", but for historical reasons
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
(define-key lisp-interaction-mode-map (kbd "C-x k") 'clear-scratch-buffer)
(global-set-key (kbd "C-x j") '(lambda () (interactive)
                                 (ido-find-file-in-dir lambda-x-direcotry)))

(setq mouse-wheel-scroll-amount '(3 ((shift) . 1)) ; three line at a time
      mouse-wheel-progressive-speed nil ; don't accelerate scrolling
      scroll-step 1
      scroll-preserve-screen-position t
      scroll-conservatively most-positive-fixnum)

;;; bookmark -------------------------------------------------------------------
(require 'bookmark)
(setq bookmark-default-file (expand-file-name "auto-save-list/bookmarks"
											  user-emacs-directory))

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

;; Evil ------- A wonderful editor in Emacs ------------------------------------
(lambda-package-ensure-install 'evil)
(evil-mode 1)
(diminish 'undo-tree-mode)

(setq evil-want-visual-char-semi-exclusive t
      ;;evil-want-C-i-jump nil
      evil-want-fine-undo t
      evil-cross-lines t)
(define-key evil-insert-state-map (kbd "C-d") 'delete-char)
(define-key evil-insert-state-map (kbd "C-e") 'move-end-of-line)
(define-key evil-insert-state-map (kbd "C-n") 'next-line)
(define-key evil-insert-state-map (kbd "C-p") 'previous-line)
(define-key evil-insert-state-map (kbd "C-t") 'transpose-chars)
(define-key evil-insert-state-map (kbd "C-w") 'evil-window-map)
(define-key evil-insert-state-map (kbd "C-y") 'yank)

;;(define-key evil-motion-state-map (kbd "C-i") 'evil-jump-forward)
(define-key evil-emacs-state-map (kbd "C-w") 'evil-window-map)

(define-key evil-window-map (kbd "w") 'ace-window)
(define-key evil-window-map (kbd "C-w") 'ace-window)

;; Expand-region ---------------------------------------------------------------
(lambda-package-ensure-install 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
(if (featurep 'evil-leader)
	(progn
	  (setq expand-region-contract-fast-key "z")
	  (evil-leader/set-key "xx" 'er/expand-region)))

;; evil-exchange ---------------------------------------------------------------
;; gx (evil-exchange)
;; gX (evil-exchange-cancel)
;; evil-exchange can be used with ace-jump, it's perfect
(lambda-package-ensure-install 'evil-exchange)
(require 'evil-exchange)
(evil-exchange-install)

;; evil-matchit ----------------------------------------------------------------
(lambda-package-ensure-install 'evil-matchit)
(require 'evil-matchit)
(global-evil-matchit-mode 1)

;; evil-visualstar ------------------------------------------------------------
(lambda-package-ensure-install 'evil-visualstar)
(require 'evil-visualstar)

;; evil-leader -----------------------------------------------------------------
(lambda-package-ensure-install 'evil-leader)
(require 'evil-leader)
(evil-leader/set-leader "<SPC>")
(if (featurep 'helm)
	(evil-leader/set-key
	  "e" 'helm-find-files
	  "b" 'helm-buffers-list)
  (evil-leader/set-key
	"e" 'find-file
	"b" 'switch-to-buffer))
(evil-leader/set-key
  "k" 'kill-this-buffer)
(global-evil-leader-mode 1)

;; auto-complete ---------------------------------------------------------------
(lambda-package-ensure-install 'auto-complete)
(global-auto-complete-mode 1)
(diminish 'auto-complete-mode)
(require 'auto-complete-config)
(ac-config-default)
(add-to-list 'ac-dictionary-directories
             (concat lambda-x-direcotry "ac-dict"))
(add-to-list 'ac-dictionary-files
             (concat lambda-x-direcotry "ac-dict/auto-complete.dict"))
(setq ac-auto-start 1 
      ac-comphist-file (expand-file-name "auto-save-list/ac-comphist.dat"
										 user-emacs-directory)
      ac-modes 
      (append
       ac-modes
       '(shell-mode
         graphviz-dot-mode conf-xdefaults-mode html-mode nxml-mode objc-mode
         sql-mode change-log-mode text-mode makefile-gmake-mode
         makefile-bsdmake-mo autoconf-mode makefile-automake-mode
         snippet-mode eshell-mode))
      ac-use-menu-map t)

(setq-default ac-sources (append '(ac-source-filename ac-source-yasnippet)
                                 ac-sources))

;;(define-key ac-mode-map (kbd "M-/") 'auto-complete)
(define-key ac-completing-map (kbd "<tab>") 'ac-expand)
(define-key ac-completing-map (kbd "<backtab>") 'ac-previous)
(define-key ac-completing-map (kbd "<return>") 'ac-complete)
(defun ac-pcomplete ()
  ;; eshell uses `insert-and-inherit' to insert a \t if no completion
  ;; can be found, but this must not happen as auto-complete source
  (flet ((insert-and-inherit (&rest args)))
    ;; this code is stolen from `pcomplete' in pcomplete.el
    (let* (tramp-mode ;; do not automatically complete remote stuff
           (pcomplete-stub)
           (pcomplete-show-list t) ;; inhibit patterns like * being deleted
           pcomplete-seen pcomplete-norm-func
           pcomplete-args pcomplete-last pcomplete-index
           (pcomplete-autolist pcomplete-autolist)
           (pcomplete-suffix-list pcomplete-suffix-list)
           (candidates (pcomplete-completions))
           (beg (pcomplete-begin))
           ;; note, buffer text and completion argument may be
           ;; different because the buffer text may bet transformed
           ;; before being completed (e.g. variables like $HOME may be
           ;; expanded)
           (buftext (buffer-substring beg (point)))
           (arg (nth pcomplete-index pcomplete-args)))
      ;; we auto-complete only if the stub is non-empty and matches
      ;; the end of the buffer text
      (when (and (not (zerop (length pcomplete-stub)))
                 (or (string= pcomplete-stub ; Emacs 23
                              (substring buftext
                                         (max 0
                                              (- (length buftext)
                                                 (length pcomplete-stub)))))
                     (string= pcomplete-stub ; Emacs 24
                              (substring arg
                                         (max 0
                                              (- (length arg)
                                                 (length pcomplete-stub)))))))
        ;; Collect all possible completions for the stub. Note that
        ;; `candidates` may be a function, that's why we use
        ;; `all-completions`.
        (let* ((cnds (all-completions pcomplete-stub candidates))
               (bnds (completion-boundaries pcomplete-stub
                                            candidates
                                            nil
                                            ""))
               (skip (- (length pcomplete-stub) (car bnds))))
          ;; We replace the stub at the beginning of each candidate by
          ;; the real buffer content.
          (mapcar #'(lambda (cand) (concat buftext (substring cand skip)))
                  cnds))))))

(defvar ac-source-pcomplete
  '((candidates . ac-pcomplete)))
;; pos-tip ---------------------------------------------------------------------
(lambda-package-ensure-install 'pos-tip)
(require 'pos-tip)
(setq ac-quick-help-prefer-pos-tip t) 

;; YASnippet -------------------------------------------------------------------
(lambda-package-ensure-install 'yasnippet)
(require 'yasnippet)
(add-to-list 'yas-snippet-dirs (concat lambda-x-direcotry "snippets"))
;; Delete dirs that don't exist in yas-snippet-dirs
(dolist (dir yas-snippet-dirs)
  (unless (file-directory-p dir)
    (setq yas-snippet-dirs (delete dir yas-snippet-dirs))))

(setq yas-trigger-key "<tab>"
      yas-next-field-key "<tab>"
      ;; menu only show the mode according to the major-mode of the current
      ;;buffer
      yas-use-menu 'abbreviate)

;;popup ------------------------------------------------------------------------
(lambda-package-ensure-install 'popup)
(require 'popup)
;; add some shotcuts in popup menu mode
(define-key popup-menu-keymap (kbd "M-n") 'popup-next)
(define-key popup-menu-keymap (kbd "<tab>") 'popup-next)
(define-key popup-menu-keymap (kbd "<backtab>") 'popup-previous)
(define-key popup-menu-keymap (kbd "M-p") 'popup-previous)
(define-key popup-menu-keymap (kbd "<escape>") '(lambda nil
                                                  (interactive)
                                                  (evil-force-normal-state)
                                                  (keyboard-quit)))

(defun yas-popup-isearch-prompt (prompt choices &optional display-fn)
  (when (featurep 'popup)
    (popup-menu*
     (mapcar
      (lambda (choice)
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

;; Ack A better grep for programmers -------------------------------------------
(lambda-package-ensure-install 'ack-and-a-half)
;; Put the ack script in no-elpa
(setq ack-and-a-half-executable (concat lambda-x-direcotry "non-elpa/ack"))
(setq ack-and-a-half-use-ido t)
;; Create shorter aliases
(defalias 'ack 'ack-and-a-half)
(defalias 'ack-same 'ack-and-a-half-same)
(defalias 'ack-find-file 'ack-and-a-half-find-file)
(defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)

;; projectile is a project management mode -------------------------------------
(lambda-package-ensure-install 'projectile)
(require 'projectile)
(setq projectile-cache-file (expand-file-name 
							 "auto-save-list/projectile.cache"
							 user-emacs-directory)
	  projectile-known-projects-file (expand-file-name 
									  "auto-save-list/projectile-bookmarks.eld"
									  user-emacs-directory))
(projectile-global-mode t)
;;(diminish 'projectile-mode "Prjl")
(diminish 'projectile-mode)

;; eshell ----------------------------------------------------------------------
(require 'eshell)
(setq eshell-directory-name (expand-file-name
							 "auto-save-list/eshell/"
							 user-emacs-directory))
;; When input things in eshell, goto the end of the buffer automatically.
(setq eshell-scroll-to-bottom-on-input 'this)
(add-hook 'eshell-mode-hook
          (lambda ()
            (add-to-list 'ac-sources 'ac-source-pcomplete)))

(add-to-list 'ac-modes 'eshell-mode)
(add-hook 'eshell-mode-hook 'turn-on-eldoc-mode)
(add-hook 'eshell-mode-hook 'ac-emacs-lisp-mode-setup)

;; unbound ---------------------------------------------------------------------
(lambda-package-ensure-install 'unbound)

;; ace jump --------------------------------------------------------------------
(lambda-package-ensure-install 'ace-jump-mode)
(require 'ace-jump-mode)
(when (and (featurep 'evil) (featurep 'evil-leader))
  (evil-leader/set-key
	"c" 'ace-jump-char-mode
	"w" 'ace-jump-word-mode
	"l" 'ace-jump-line-mode))
(lambda-package-ensure-install 'ace-window)
(global-set-key (kbd "C-x o") 'ace-window)
;;(global-set-key (kbd "M-p") 'ace-jump-buffer)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
    
;; anzu-mode enhances isearch by showing total matches and current match 
;; position --------------------------------------------------------------------
(lambda-package-ensure-install 'anzu)
(require 'anzu)
(global-anzu-mode)
(diminish 'anzu-mode)

;; highlights parentheses, brackets, and braces according to their depth--------
(lambda-package-ensure-install 'rainbow-delimiters)
(global-rainbow-delimiters-mode)

;; global ------- code navigating ----------------------------------------------
(lambda-package-ensure-install 'ggtags)
(if (featurep 'evil)
	(define-key evil-normal-state-map
	  (kbd "M-.") 'ggtags-find-tag-dwim))
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
              (ggtags-mode 1))))

;; smartparens -----------------------------------------------------------------
;; global
(lambda-package-ensure-install 'smartparens)
;; setq should before (require 'smartparens-config)
(setq sp-base-key-bindings 'sp)
(setq sp-autoskip-closing-pair 'always)
(setq sp-navigate-close-if-unbalanced t)
;; (setq sp-show-pair-from-inside t)
(require 'smartparens-config)
(define-key smartparens-strict-mode-map
  [remap c-electric-backspace] 'sp-backward-delete-char)
;; use smartparens key bindings
(smartparens-global-mode t)
(smartparens-global-strict-mode t)
;; (show-smartparens-global-mode t)
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

(dolist (mode '(c-mode c++-mode java-mode js2-mode sh-mode css-mode))
  (sp-local-pair mode
				 "{"
				 nil
				 :post-handlers
				 '((ome-create-newline-and-enter-sexp "RET"))))

;; unicad --- say goodbye to Garbled -------------------------------------------
(require 'unicad)

;; Issues: ---------------------------------------------------------------------

(provide 'lambda-core)
;;; lambda-core.el ends here
