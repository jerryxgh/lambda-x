;;; lambda-dired-subtree.el ---  -*- lexical-binding: t -*-

;; Time-stamp: <2025-02-17 15:09:17 Guanghui Xu>

;;; Commentary:
;; Configuration for dired-subtree.

;;; Code:

(require 'lambda-treemacs)

(use-package dired-subtree
  :ensure t
  :bind (:map dired-mode-map
              ("<tab>" . dired-subtree-cycle)
              ("TAB" . dired-subtree-cycle)))

(use-package evil-collection
  :ensure t
  :custom
  ;; minibuffer use emacs default key bindings
  ;; (evil-collection-setup-minibuffer t)
  (evil-collection-outline-enable-in-minor-mode-p nil)
  :config
  (setq evil-collection-mode-list
        (seq-remove (lambda (item) (memq item '(comint company)))
                    evil-collection-mode-list))
  (evil-collection-init))

(provide 'lambda-dired-subtree)

;;; lambda-dired-subtree.el ends here
