;;; lambda-session.el --- auto save and load session

;;; Commentary:
;; This should be loaded at last, restore buffers, minibuffer history, last
;; place of cursor

;;; Code:

(require 'lambda-core)
(require 'lambda-evil)

;; savehist keeps track of some history ----------------------------------------
;; very useful
(require 'savehist)
(setq savehist-additional-variables '(search ring regexp-search-ring)
      savehist-autosave-interval 60 ; save every minute
      savehist-file (expand-file-name ; keep the home clean
                     "savehist"
                     lambda-auto-save-dir))
(savehist-mode 1)

;; save recent files -----------------------------------------------------------
;; very useful
(require 'recentf)
(setq recentf-save-file (expand-file-name
                         "recentf"
                         lambda-auto-save-dir)
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
;; to keep home clean
(setq save-place-file (expand-file-name
                       "savedplace"
                       lambda-auto-save-dir))
;; activate it for all buffers
(setq-default save-place t)

;; elscreen manage tabs --------------------------------------------------------
;; (lambda-package-ensure-install 'elscreen)
;; (require 'elscreen)
;; (setq elscreen-display-tab nil
;;       elscreen-prefix-key (kbd "C-;"))
;; (elscreen-start)
;; (define-key evil-normal-state-map (kbd "g t") 'elscreen-next)
;; (define-key evil-normal-state-map (kbd "g T") 'elscreen-previous)

;; (lambda-package-ensure-install 'elscreen-persist)
;; (require 'elscreen-persist)
;; (setq elscreen-persist-file (expand-file-name "elscreen"
;;                                               lambda-auto-save-dir))
;; (elscreen-persist-mode 1)

;; persp-mode - replace elscreen -----------------------------------------------
;; TODO: compare to perspective-el
(lambda-package-ensure-install 'persp-mode)
(setq persp-keymap-prefix (kbd "C-;")
      persp-save-dir (expand-file-name "persp-confs" lambda-auto-save-dir)
      )

(defun persp-desktop-ignore-this-minor-mode (buffer)
  "Installed as a minor-mode initializer for Desktop mode.
BUFFER is the buffer to not initialize a Semantic minor mode in."
  nil)

;; (add-to-list 'desktop-minor-mode-handlers
;;              '(persp-mode . persp-desktop-ignore-this-minor-mode))

(with-eval-after-load "persp-mode-autoloads"
  (setq persp-autokill-buffer-on-remove 'kill-weak
        persp-kill-foreign-buffer-action 'kill)
  (add-hook
   'after-init-hook
   #'(lambda ()
       (add-hook
        'persp-mode-hook
        #'(lambda ()
            (setq persp-interactive-completion-function #'ido-completing-read)))

       (persp-mode 1)
       (diminish 'persp-mode))))

;; window zoom -----------------------------------------------------------------
;; enlarge current window temporarily
(lambda-package-ensure-install 'zoom-window)
;; (require 'zoom-window)
;; (setq zoom-window-use-elscreen t)
(setq zoom-window-use-persp t
      ;; zoom-window-mode-line-color "black"
      zoom-window-mode-line-color "DarkGreen")
(zoom-window-setup)
(global-set-key (kbd "C-x C-z") 'zoom-window-zoom)

;; Restore buffers automaticly -------------------------------------------------
(require 'desktop)
(setq desktop-path (list (expand-file-name lambda-auto-save-dir))
      history-length 250
      desktop-restore-frames t
      desktop-dirname (expand-file-name lambda-auto-save-dir)
      desktop-files-not-to-save (concat desktop-files-not-to-save "\\|.*\\.gpg$")
      desktop-base-file-name "emacs-desktop")

(desktop-save-mode 1)

;;加快emacs的启动速度
(if (or (eq system-type 'darwin) (eq system-type 'gnu/linux))
    (server-start))

(provide 'lambda-session)

;;; lambda-session.el ends here
