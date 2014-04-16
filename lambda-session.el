;;; lambda-session.el --- auto save and load session

;;; Commentary:
;; This should be loaded at last, restore buffers, minibuffer history, last
;; place of cursor

;;; Code:

(require 'lambda-core)

;; Restore buffers automaticly -------------------------------------------------
(require 'desktop)
(setq desktop-path (list (expand-file-name "auto-save-list/"
										   user-emacs-directory)))

(setq desktop-dirname (expand-file-name "auto-save-list/"
										user-emacs-directory))
(setq desktop-base-file-name "emacs-desktop")
(desktop-save-mode 1)

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
(setq wg-default-session-file (locate-user-emacs-file
							   "auto-save-list/emacs-workgroups"))

;; put this one at the bottom of this file
(workgroups-mode 1)
(diminish 'workgroups-mode)

;; Maxmize frame(Full Screen) --------------------------------------------------
(defun lambda-full-screen ()
  "Make Emacs full-screen after init."
  (if (and (eq system-type 'windows-nt)
		   (fboundp 'w32-send-sys-command))
	  (w32-send-sys-command 61488)))
(add-hook 'after-init-hook 'lambda-full-screen)


(provide 'lambda-session)

;;; lambda-session.el ends here
