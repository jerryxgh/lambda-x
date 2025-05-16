;;; lambda-session.el --- auto save and load session

;;; Commentary:
;; This should be loaded at last, restore buffers, minibuffer history, last
;; place of cursor

;;; Code:

(require 'lambda-core)
(require 'lambda-evil)

;; savehist keeps track of some history ----------------------------------------
(use-package savehist
  :init
  (savehist-mode)
  (setq savehist-additional-variables '(search ring regexp-search-ring)
        savehist-autosave-interval 60 ; save every minute
        savehist-file (expand-file-name ; keep the home clean
                       "savehist"
                       lambda-auto-save-dir)))

;; save recent files -----------------------------------------------------------
;; very useful
(require 'recentf)
(setq recentf-save-file (expand-file-name
                         "recentf"
                         lambda-auto-save-dir)
      recentf-max-saved-items nil ; save whole list
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
;; to keep home clean
(setq save-place-file (expand-file-name
                       "savedplace"
                       lambda-auto-save-dir))
;; activate it for all buffers
(setq-default save-place t)

;; persp-mode - replace elscreen -----------------------------------------------
(use-package persp-mode
  :ensure t
  :diminish persp-mode
  :custom
  (persp-keymap-prefix (kbd "C-;"))
  (persp-save-dir (expand-file-name "persp-confs" lambda-auto-save-dir))
  :config
  (with-eval-after-load "persp-mode-autoloads"
    ;; work with workgroups.el
    (setq wg-morph-on nil)
    ;; switch off the animation of restoring window configuration
    (setq persp-autokill-buffer-on-remove 'kill-weak)
    (add-hook 'after-init-hook (lambda () (persp-mode 1)))))

(defun persp-desktop-ignore-this-minor-mode (buffer)
  "Installed as a minor-mode initializer for Desktop mode.
BUFFER is the buffer to not initialize a Semantic minor mode in."
  nil)

;; (add-to-list 'desktop-minor-mode-handlers
;;              '(persp-mode . persp-desktop-ignore-this-minor-mode))

(with-eval-after-load 'persp-mode-autoloads
  (setq persp-autokill-buffer-on-remove 'kill-weak
        persp-kill-foreign-buffer-action 'kill)
  (add-hook
   'after-init-hook
   #'(lambda ()
       (persp-mode 1)
       (diminish 'persp-mode))))

;; window zoom -----------------------------------------------------------------
;; enlarge current window temporarily
(use-package zoom-window
  :ensure t
  :bind ("C-x C-z" . 'zoom-window-zoom)
  :custom
  ;; depends on persp-mode
  (zoom-window-use-persp t)
  (zoom-window-mode-line-color "DarkGreen")
  :config
  (zoom-window-setup))

;; Restore buffers automaticly -------------------------------------------------
(require 'desktop)
(use-package desktop
  :ensure t

  :custom
  (desktop-path (list (expand-file-name lambda-auto-save-dir)))
  (history-length 100)
  (desktop-restore-frames t)
  (desktop-files-not-to-save (concat desktop-files-not-to-save "\\|.*\\.gpg$"))
  (desktop-base-file-name "emacs-desktop")

  :config
  (desktop-save-mode 1))

;;加快emacs的启动速度
(if (or (eq system-type 'darwin) (eq system-type 'gnu/linux))
    (server-start))

(provide 'lambda-session)

;;; lambda-session.el ends here
