;; lambda-evil.el --- configuration for evil
;; Time-stamp: <2015-09-21 10:30:41 Jerry Xu>

;;; Commentary:
;; Configuration for evil.

;;; Code:

;; evil ------- A wonderful editor in Emacs ------------------------------------
(lambda-package-ensure-install 'evil)
(require 'evil)
(evil-mode 1)
(diminish 'undo-tree-mode)

(setq evil-want-visual-char-semi-exclusive t
      ;; evil-want-C-i-jump nil
      evil-want-C-w-delete t
      evil-auto-balance-windows nil
      evil-cross-lines t)
;; let * and # search symbol instead of word at point
(setq-default evil-symbol-word-search t)
;; settings below restore key bindings in emacs in insert state
(define-key evil-insert-state-map (kbd "C-S-k") 'evil-insert-digraph)
(define-key evil-insert-state-map (kbd "C-a") 'move-beginning-of-line)
(define-key evil-insert-state-map (kbd "C-b") 'backward-char)
(define-key evil-insert-state-map (kbd "C-d") 'delete-char)
(define-key evil-insert-state-map (kbd "C-e") 'move-end-of-line)
(define-key evil-insert-state-map (kbd "C-f") 'forward-char)
(define-key evil-insert-state-map (kbd "C-k") 'kill-line)
(define-key evil-insert-state-map (kbd "C-n") 'next-line)
(define-key evil-insert-state-map (kbd "C-p") 'previous-line)
(define-key evil-insert-state-map (kbd "C-t") 'transpose-chars)
;; (define-key evil-insert-state-map (kbd "C-w") 'evil-window-map)
(define-key evil-insert-state-map (kbd "C-y") 'yank)

(define-key evil-motion-state-map (kbd "C-a") 'move-beginning-of-line)
(define-key evil-motion-state-map (kbd "C-e") 'move-end-of-line)
(define-key evil-motion-state-map (kbd "C-k") 'kill-line)

(defun lambda-hs-hide-level-1 ()
  "Just fold level 1 elements."
  (hs-hide-level 1))
(define-key evil-normal-state-map (kbd "zM")
  '(lambda ()
     (interactive)
     (let ((hs-hide-all-non-comment-function 'lambda-hs-hide-level-1))
       (evil-close-folds))))

(defun lambda-copy-to-end-of-line ()
  "Copy to end of line, and bind this funciton to Y in normal mode."
  (interactive)
  (evil-yank (point) (point-at-eol)))
(define-key evil-normal-state-map (kbd "Y") 'lambda-copy-to-end-of-line)
(define-key evil-normal-state-map (kbd "g f") 'lambda-ido-find-file-at-point)

(mapc #'(lambda (mode-state-pair)
          (evil-set-initial-state (car mode-state-pair) (cdr mode-state-pair)))
      '((calendar-mode . emacs)
        (help-mode . emacs)
        (Info-mode . emacs)
        (dired-mode . emacs)
        (Man-mode . emacs)
        (grep-mode . emacs)
        (view-mode . emacs)
        (ack-mode . emacs)
        (image-mode . emacs)))

;; when entering edebug, change to evil-emacs-state to use simple key bindings
;; (require 'edebug)
;; (add-hook 'edebug-mode-hook (lambda ()
;;                               (if edebug-mode
;;                                   (evil-emacs-state)
;;                                 (evil-normal-state))))
(add-hook 'edebug-mode-hook 'evil-normalize-keymaps)

;;(define-key evil-motion-state-map (kbd "C-i") 'evil-jump-forward)
(define-key evil-emacs-state-map (kbd "C-w") 'evil-window-map)

(define-key evil-window-map (kbd "w") 'ace-window)
(define-key evil-window-map (kbd "C-w") 'ace-window)

(require 'evil-tab-minor-mode)
(global-evil-tab-mode t)

;; expand-region ---------------------------------------------------------------
(lambda-package-ensure-install 'expand-region)
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
(if (featurep 'evil-leader)
    (progn
      (setq expand-region-contract-fast-key "z")
      (evil-leader/set-key "xx" 'er/expand-region)))

;; evil-exchange ---------------------------------------------------------------
;; powerful tool to exchange text
;; gx (evil-exchange)
;; gX (evil-exchange-cancel)
;; evil-exchange can be used with ace-jump, it's perfect
(lambda-package-ensure-install 'evil-exchange)
(require 'evil-exchange)
(evil-exchange-install)

;; evil-matchit ----------------------------------------------------------------
;; Jump between beginning and ending of structure like parens, html tags etc..
(lambda-package-ensure-install 'evil-matchit)
(require 'evil-matchit)
(global-evil-matchit-mode 1)

;; evil-visualstar -------------------------------------------------------------
(lambda-package-ensure-install 'evil-visualstar)
(require 'evil-visualstar)
(global-evil-visualstar-mode t)

;; evil-leader -----------------------------------------------------------------
(lambda-package-ensure-install 'evil-leader)
(require 'evil-leader)
(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
  "b" 'ido-switch-buffer
  "e" 'helm-projectile
  "k" 'kill-this-buffer
  "o" 'helm-occur
  "f" 'find-file)
(global-evil-leader-mode 1)

;; evil-surround ---------------------------------------------------------------
;; add surrounding
;; visual-state: S<textobject><trigger>, normal-state: ys<textobject><trigger>.

;; change surrounding
;; cs<old-trigger><new-trigger>

;; delete surrounding
;; ds<trigger>.

(lambda-package-ensure-install 'evil-surround)
(require 'evil-surround)
(global-evil-surround-mode 1)

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
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

(lambda-package-ensure-install 'evil-commentary)
(evil-commentary-default-setup)

(lambda-package-ensure-install 'evil-numbers)
(require 'evil-numbers)
(define-key evil-normal-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt)
(define-key evil-visual-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt)
(define-key evil-visual-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt)

;; anzu for evil ---------------------------------------------------------------
(lambda-package-ensure-install 'evil-anzu)
(require 'evil-anzu)

;; evil-visual-mark-mode -------------------------------------------------------
(lambda-package-ensure-install 'evil-visual-mark-mode)
(evil-visual-mark-mode 1)

;; evil-jumper jumper like vim -------------------------------------------------
(lambda-package-ensure-install 'evil-jumper)
(global-evil-jumper-mode 1)

;; evil-indent-textobject ------------------------------------------------------
(lambda-package-ensure-install 'evil-indent-textobject)

;; evil-smartparens ------------------------------------------------------
(lambda-package-ensure-install 'evil-smartparens)
(require 'evil-smartparens)
(evil-smartparens-mode 1)

(provide 'lambda-evil)

;;; lambda-evil.el ends here
