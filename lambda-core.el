;;; lambda-core.el --- 
;; Time-stamp: <2014-04-10 19:02:00 Jerry Xu>

;; Miscellaneous basic settings ------------------------------------------------
(setq user-full-name "Jerry Xu"
      user-mail-address "jerryxgh@gmail.com"
      inhibit-startup-screen t 
      abbrev-file-name "~/.emacs.d/auto-save-list/.abbrev_defs" 
      custom-file (concat lambda-x-direcotry "lambda-custom.el")
      use-dialog-box nil 
      sql-mysql-options '("-C" "-t" "-f" "-n" "--default-character-set=utf8") 
      make-backup-files nil 
      resize-mini-windows t 
      ring-bell-function 'ignore ; inhibit annoying warning sound
      x-select-enable-clipboard t 
      enable-recursive-minibuffers t 
      ;;confirm-kill-emacs 'y-or-n-p 
      ;; TODO: move it to appropriate palce
      ;;inhibit-eol-conversion t
      )

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
(global-auto-revert-mode 1) 
;;(global-subword-mode 1)
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
(tooltip-mode 0) 
;;(global-hl-line-mode 1) ; highlight current line
(scroll-bar-mode 0) 
;; the toolbar is just a waste of valuable screen estate
;; in a tty tool-bar-mode does not properly auto-load, and is
;; already disabled anyway
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(set-default-font "Consolas-11") 
(set-background-color "#CCE8CF") 
(set-fontset-font t 'unicode '("Microsoft Yahei" .  "unicode-bmp"))
(setq-default indicate-buffer-boundaries '((top . left) (t . right))
              indicate-empty-lines t)

;; saveplace --- When you visit a file, point goes to the last place where
;; it was when you previously visited the same file.----------------------------
(require 'saveplace)
;; Your saved places are written to the file stored in the file specified by
;; save-place-file. This defaults to ~/.emacs-places. You may want to change it
;; to keep your home directory uncluttered, for example:
(setq save-place-file (expand-file-name 
					   "auto-save-list/saved-places"
					   user-emacs-directory))
;; activate it for all buffers
(setq-default save-place t)
      ;; TODO: move it to appropriate palce

;; eshell ----------------------------------------------------------------------
(require 'eshell)
(setq eshell-directory-name (expand-file-name
							 "auto-save-list/.eshell/"
							 user-emacs-directory))

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

;; savehist keeps track of some history ----------------------------------------
(require 'savehist)
(setq savehist-additional-variables
      ;; search entries
      '(search ring regexp-search-ring)
      ;; save every minute
      savehist-autosave-interval 60
      ;; keep the home clean
      savehist-file (expand-file-name
					 "auto-save-list/savehist"
					 user-emacs-directory))
(savehist-mode 1)
;; save recent files -----------------------------------------------------------
(require 'recentf)
(setq recentf-save-file (expand-file-name
						 "auto-save-list/recentf"
						 user-emacs-directory)
      recentf-max-saved-items 500
      recentf-max-menu-items 15)
(recentf-mode 1)
;; use shift + arrow keys to switch between visible buffers --------------------
(require 'windmove)
(windmove-default-keybindings)
;; uniquify --- easy to distinguish same name buffers---------------------------
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

;; sensible undo ---------------------------------------------------------------
(lambda-package-ensure-install 'undo-tree)
(global-undo-tree-mode 1)
(diminish 'undo-tree-mode)

;; diminish keeps the modeline tidy --------------------------------------------
(lambda-package-ensure-install 'diminish)
(require 'diminish)

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
  "create a scratch buffer"
  (interactive)
  (unless (get-buffer "*scratch*")
    (switch-to-buffer (get-buffer-create "*scratch*"))
    (insert initial-scratch-message)
    (goto-char (point-max))
    (lisp-interaction-mode)))

(defun clear-scratch-buffer nil 
  "Clear the scratch buffer."
  (interactive)
  (when (equal (buffer-name (current-buffer)) "*scratch*")
    (delete-region (point-min) (point-max))
    (insert initial-scratch-message)
    (goto-char (point-max))))

(defun interactive-shell-on-exit-kill-buffer nil
  "kill the buffer on exit of interactive shell"
  (let ((process (get-buffer-process (current-buffer))))
    (if process
        (set-process-sentinel 
		 process
		 (lambda (process state)
		   (when (or (string-match "exited abnormally with code." state)
					 (string-match "finished" state))
			 (kill-buffer (current-buffer))))))))
(defun goto-previous-buffer ()
  "Back to the previous buffer."
  (interactive)
  (let ((buffer-to-switch
		 (catch 'break
		   (dolist (buffer (buffer-list (selected-frame)))
			 (unless (or (string-match "^ \\*" (buffer-name buffer))
						 (eq buffer (current-buffer)))
			   (throw 'break buffer))))))
    (if buffer-to-switch
		(switch-to-buffer-smartly buffer-to-switch)
      (message "Don't know which buffer to go..."))))

(defun toggle-eshell-buffer ()
  "If current buffer is eshell buffer, switch to previous buffer.
If current buffer is not eshell buffer, switch to eshell buffer,
if that does'texists, create one and swich to it."
  (interactive)
  (toggle-shell-buffer eshell-buffer-name 'eshell)
  )

(defun toggle-shell-buffer (shell-buffer-name shell-new-command)
  (if (equal (buffer-name (current-buffer)) shell-buffer-name)
      (goto-previous-buffer)
    (let ((shell-buffer (get-buffer shell-buffer-name)))
      (if shell-buffer
		  (switch-to-buffer-smartly shell-buffer)
        (if (functionp shell-new-command)
            (funcall shell-new-command))))))

(defun switch-to-buffer-smartly (buffer-to-switch)
  "Switch to buffer, if buffer has been shown in a window, goto that window, \
 else act as switch-to-buffer"
  (let ((window-to-select
		 (catch 'found
		   (walk-windows #'(lambda (window)
							 (if (eq buffer-to-switch (window-buffer window))
								 (throw 'found window)))))))
    (if window-to-select
		(select-window window-to-select)
      (switch-to-buffer buffer-to-switch))))
;;;             
(defun bookmark-completing-read (prompt &optional default)
  "Prompting with PROMPT, read a bookmark name in ido completion.
PROMPT will get a \": \" stuck on the end no matter what, so you
probably don't want to include one yourself.
Optional second arg DEFAULT is a string to return if the user enters
the empty string."
  (bookmark-maybe-load-default-file) ; paranoia
  (if (listp last-nonmenu-event)
      (bookmark-menu-popup-paned-menu t prompt
									  (if bookmark-sort-flag
										  (sort (bookmark-all-names)
												'string-lessp)
										(bookmark-all-names)))
    (let* ((completion-ignore-case bookmark-completion-ignore-case)
		   (default default)
		   (prompt (concat prompt (if default
                                      (format " (%s): " default)
                                    ": ")))
		   (str
			(ido-completing-read prompt
                                 (mapcar 'car bookmark-alist)
                                 nil
                                 0
                                 nil
                                 'bookmark-history)))
      (if (string-equal "" str) default str))))

(load "lambda-custom") 

(add-hook 'comint-mode-hook 'interactive-shell-on-exit-kill-buffer)

;;; ido --- interactively do things---------------------------------------------
(lambda-package-ensure-install 'ido-ubiquitous)
(lambda-package-ensure-install 'flx-ido)
(require 'ido)
(require 'ido-ubiquitous)
(require 'flx-ido)
(setq ido-enable-flex-matching t
      ido-auto-merge-work-directories-length -1
      ido-save-directory-list-file
	  (expand-file-name "auto-save-list/.ido.last" user-emacs-directory)
      ido-default-file-method 'selected-window)
;;(setq ido-ignore-buffers  '("\\` " "^\\*.*\\*$"))
(ido-mode t)
(ido-ubiquitous-mode 1)
;;; smarter fuzzy matching for ido
(flx-ido-mode +1)

;;; smex, remember recently and most frequently used commands ------------------
(lambda-package-ensure-install 'smex)
(require 'smex)
(setq smex-save-file (expand-file-name "auto-save-list/.smex-items"
									   user-emacs-directory))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;;; tramp ----------------------------------------------------------------------
;; Usage: type `C-x C-f' and then enter the filename`/user@machine:/path/to.file
(require 'tramp)
(when (> emacs-major-version 23)
  (require 'tramp-sh)
  (delete "LC_ALL=C" tramp-remote-process-environment)
  (add-to-list 'tramp-remote-process-environment "LANG=zh_CN.utf8" 'append)
  (add-to-list 'tramp-remote-process-environment "LC_ALL=zh_CN.utf8" 'append))

(setq tramp-auto-save-directory  "~/.emacs.d/auto-save-list/tramp"
      ido-enable-tramp-completion t)
(if (eq system-type 'windows-nt)
    (setq tramp-default-method "plink"))

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
  (if (and (eq system-type 'windows-nt)
           (file-exists-p git-executable-windows))
      (setq magit-git-executable git-executable-windows)))

;;; dired-x --------------------------------------------------------------------
(require 'dired-x)

;;; fill-column ----------------------------------------------------------------
(setq-default fill-column 80)
;;(require 'fill-column-indicator)
(lambda-package-ensure-install 'fill-column-indicator)

;; key-bindings for myself, you can change this to yours
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-\\") 'toggle-truncate-lines) ;; ¡Á           
(global-set-key (kbd "C-<") 'shrink-window)
(global-set-key (kbd "C->") 'enlarge-window)
(define-key global-map (kbd "C-x C-z") 'goto-previous-buffer)
(define-key global-map (kbd "<f7>") 'toggle-eshell-buffer)
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

;; redefine function bookmark-completing-read to use ido
(defun bookmark-completing-read (prompt &optional default)
  "Prompting with PROMPT, read a bookmark name in completion.
  PROMPT will get a \": \" stuck on the end no matter what, so you
  probably don't want to include one yourself.
  Optional second arg DEFAULT is a string to return if the user enters
  the empty string."
  (bookmark-maybe-load-default-file) ; paranoia
  (if (listp last-nonmenu-event)
      (bookmark-menu-popup-paned-menu t prompt
                                      (if bookmark-sort-flag
                                          (sort (bookmark-all-names)
                                                'string-lessp)
                                        (bookmark-all-names)))
    (let* ((completion-ignore-case bookmark-completion-ignore-case)
           (default default)
           (prompt (concat prompt (if default
                                      (format " (%s): " default)
                                    ": ")))
           (str
            (ido-completing-read prompt
                                 (mapcar 'car bookmark-alist)
                                 nil
                                 0
                                 nil
                                 'bookmark-history)))
      (if (string-equal "" str) default str))))

(defun clear ()
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

(define-key evil-motion-state-map (kbd "C-i") 'evil-jump-forward)
(define-key evil-emacs-state-map (kbd "C-w") 'evil-window-map)

(define-key evil-window-map (kbd "w") 'ace-window)
(define-key evil-window-map (kbd "C-w") 'ace-window)

;; auto-complete ---------------------------------------------------------------
(lambda-package-ensure-install 'auto-complete)
(global-auto-complete-mode 1)
(require 'auto-complete-config)
(ac-config-default)
(add-to-list 'ac-dictionary-directories
             (concat lambda-x-direcotry "ac-dict"))
(add-to-list 'ac-dictionary-files
             (concat lambda-x-direcotry "ac-dict/auto-complete.dict"))
(setq ac-auto-start 1 
      ac-comphist-file "~/.emacs.d/auto-save-list/ac-comphist.dat"
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

(define-key ac-mode-map (kbd "M-/") 'auto-complete)
(define-key ac-completing-map (kbd "<tab>") 'ac-expand)
(define-key ac-completing-map (kbd "<backtab>") 'ac-previous)
(define-key ac-completing-map (kbd "<return>") 'ac-complete)

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
      yas-use-menu 'abbreviate
      )
(lambda-package-ensure-install 'popup)
;;popup ------------------------------------------------------------------------
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

;; ag --- front end of The Silver Searcher -------------------------------------
(lambda-package-ensure-install 'ag)
(setq ag-highlight-search t)
(setq ag-reuse-window 't)
;; change ag executable path
;;(setq ag-executable "C:/Wherever/I/Installed/Ag/ag.exe")

;; projectile is a project management mode -------------------------------------
(lambda-package-ensure-install 'projectile)
(require 'projectile)
(setq projectile-cache-file (expand-file-name 
							 "auto-save-list/projectile.cache"
							 user-emacs-directory))
(projectile-global-mode t)

;; helm ------------------------------------------------------------------------
(lambda-package-ensure-install 'helm)
(lambda-package-ensure-install 'helm-projectile)
(require 'helm-config)
(require 'helm-projectile)

;; unbound ---------------------------------------------------------------------
(lambda-package-ensure-install 'unbound)

;; ace jump --------------------------------------------------------------------
(lambda-package-ensure-install 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
(define-key evil-normal-state-map (kbd "SPC") 'ace-jump-mode)
(require 'ace-jump-mode)
(lambda-package-ensure-install 'ace-window)
(global-set-key (kbd "C-x o") 'ace-window)
(global-set-key (kbd "M-p") 'ace-jump-buffer)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
    
;; anzu-mode enhances isearch by showing total matches and current match 
;; position --------------------------------------------------------------------
(lambda-package-ensure-install 'anzu)
(require 'anzu)
(diminish 'anzu-mode)
(global-anzu-mode)

;; unicad --- say goodbye to Garbled -------------------------------------------
(require 'unicad)


;; Issues: ---------------------------------------------------------------------


(provide 'lambda-core)
;;; lambda-core.el ends here
