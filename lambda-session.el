;;; lambda-session.el --- auto save and load session -*- lexical-binding: t -*-

;;; Commentary:
;; This should be loaded at last, restore buffers, minibuffer history, last
;; place of cursor

;;; Code:

(require 'lambda-core)
(require 'lambda-evil)

;; savehist keeps track of some history ----------------------------------------
(use-package savehist
  :ensure nil
  :custom
  (savehist-autosave-interval 60)
  (savehist-file (expand-file-name "savehist" lambda-auto-save-dir))
  :init
  (savehist-mode 1))

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
;; Activate it for all buffers.
(save-place-mode 1)

;; persp-mode - replace elscreen -----------------------------------------------
(use-package persp-mode
  :ensure t
  :diminish persp-mode
  :custom
  (persp-keymap-prefix "C-;")
  (persp-save-dir (expand-file-name "persp-confs" lambda-auto-save-dir))
  (persp-autokill-buffer-on-remove 'kill-weak)
  (persp-kill-foreign-buffer-action 'kill)
  :config
  (if after-init-time
      (persp-mode 1)
    (add-hook 'after-init-hook #'lambda-enable-persp)))

(defun lambda-enable-persp ()
  "Enable perspective session restoration once startup has completed."
  (persp-mode 1))

;; A prefix available even in terminals without extended-key support.
(with-eval-after-load 'persp-mode
  (define-key persp-mode-map (kbd "C-c w") 'persp-key-map))

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

;; Perspective owns buffer/window restoration; do not also restore Desktop.
(setq history-length 100)

;; Let a daemon own its server; independent instances must not replace it.
(require 'server)
(when (and (not noninteractive)
           (not (daemonp))
           (memq system-type '(darwin gnu/linux))
           (not (server-running-p)))
  (server-start))

(provide 'lambda-session)

;;; lambda-session.el ends here
