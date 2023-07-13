;;; lambda-ido.el --- ido configs -*- lexical-binding: t -*-

;;; Commentary:
;; ido configs

;;; Code:

(require 'lambda-core)

;;; ido --- interactively do things---------------------------------------------
;; ffap - find file at point is not userful when ido-mode is on
(use-package flx-ido
  :ensure t)

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
      ido-save-directory-list-file (expand-file-name "ido.hist" lambda-auto-save-dir)
      ;; ido-everywhere t
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

(use-package ido-completing-read+
  :ensure t
  :config
  (ido-ubiquitous-mode 1))

;;; smex, remember recently and most frequently used commands ------------------
(use-package smex
  :ensure t
  :custom
  (smex-save-file (expand-file-name "smex-items" lambda-auto-save-dir))
  :config
  (smex-initialize)
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands))

;;; ido-at-point --- use ido to do completion-at-point -------------------------
(use-package ido-at-point
  :ensure t
  :config
  (ido-at-point-mode 1))

(provide 'lambda-ido)
;;; lambda-ido.el ends here
