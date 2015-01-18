;;; lambda-session.el --- auto save and load session

;;; Commentary:
;; This should be loaded at last, restore buffers, minibuffer history, last
;; place of cursor

;;; Code:

(require 'lambda-core)

;; savehist keeps track of some history ----------------------------------------
(require 'savehist)
(setq savehist-additional-variables
      ;; search entries
      '(search ring regexp-search-ring)
      ;; save every minute
      savehist-autosave-interval 60
      ;; keep the home clean
      savehist-file (expand-file-name
		     "savehist"
		     lambda-savefile-dir))
(savehist-mode 1)

;; save recent files -----------------------------------------------------------
(require 'recentf)
(setq recentf-save-file (expand-file-name
			 "recentf"
			 lambda-savefile-dir)
      recentf-max-saved-items 500
      recentf-max-menu-items 15
      ;; disable recentf-cleanup on Emacs start, because it can cause
      ;; problems with remote files
      recentf-auto-cleanup 'never)
;; ignore magit's commit message files
(add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'")
(recentf-mode 1)

;; saveplace --- When you visit a file, point goes to the last place where
;; it was when you previously visited the same file.----------------------------
(require 'saveplace)
;; Your saved places are written to the file stored in the file specified by
;; save-place-file. This defaults to ~/.emacs-places. You may want to change it
;; to keep your home directory uncluttered, for example:
(setq save-place-file (expand-file-name
		       "savedplace"
		       lambda-savefile-dir))
;; activate it for all buffers
(setq-default save-place t)

;; Maxmize frame(Full Screen) --------------------------------------------------
(defun lambda-maxmize-frame ()
  "Make Emacs frame maxmized."
  (interactive)
  (cond ((and (eq system-type 'windows-nt)
			  (fboundp 'w32-send-sys-command))
		 (w32-send-sys-command 61488))
		((eq system-type 'gnu/linux)
		 (set-frame-parameter nil 'fullscreen 'maximized))))
(add-hook 'after-init-hook 'lambda-maxmize-frame)

(defun lambda--full-screen ()
  "Make Emacs frame fullscreen.

This follows freedesktop standards, should work in X servers."
  (interactive)
  (if (eq window-system 'x)
      (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                             '(2 "_NET_WM_STATE_FULLSCREEN" 0))
    (error "Only X server is supported")))

;; Restore buffers automaticly -------------------------------------------------
(require 'desktop)
(setq desktop-path (list (expand-file-name lambda-savefile-dir))
      history-length 250
      desktop-dirname (expand-file-name lambda-savefile-dir)
      desktop-files-not-to-save (concat desktop-files-not-to-save "\\|.*\\.gpg$")
      desktop-base-file-name "emacs-desktop")

;; (desktop-save-mode 1)

;; Workgroup2 Use --------------------------------------------------------------
;; Most commands start with prefix `wg-prefix-key'.
;; You can change it before activating workgroups.
;; By default prefix is: "C-c z"

;; <prefix> <key>

;; <prefix> c    - create workgroup
;; <prefix> A    - rename workgroup
;; <prefix> k    - kill workgroup
;; <prefix> v    - switch to workgroup
;; <prefix> C-s  - save session
;; <prefix> C-f  - load session
(lambda-package-ensure-install 'workgroups2)
(require 'workgroups2)
;;Change prefix key (before activating WG)
(setq wg-prefix-key (kbd "C-c z"))

;;Change workgroups session file
(setq wg-default-session-file (expand-file-name
                               "emacs-workgroups"
                               lambda-savefile-dir))

(setq wg-associate-blacklist '("*helm mini*" "*Messages*" "*helm action*"))

;; put this one at the bottom of this file
(workgroups-mode 1)
(diminish 'workgroups-mode)

;; psession Persistent save of elisp objects -----------------------------------
(lambda-package-ensure-install 'psession)
(require 'psession)
(setq psession-elisp-objects-default-directory
      (expand-file-name
       "elisp-objects"
       lambda-savefile-dir))
;;加快emacs的启动速度
(server-start)

(provide 'lambda-session)

;;; lambda-session.el ends here
