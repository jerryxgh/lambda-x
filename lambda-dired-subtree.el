;;; lambda-dired-subtree.el ---  -*- lexical-binding: t -*-

;; Time-stamp: <2025-02-17 15:09:17 Guanghui Xu>

;;; Commentary:
;; Configuration for dired-subtree.

;;; Code:

(require 'lambda-evil)

(use-package dired-subtree
  :ensure t
  :bind (:map dired-mode-map
              ("<tab>" . dired-subtree-cycle)
              ("TAB" . dired-subtree-cycle))
  :config
  ;; Override Evil Collection's higher-priority subtree toggle binding.
  (evil-define-key 'normal dired-mode-map
    (kbd "<tab>") #'dired-subtree-cycle
    (kbd "TAB") #'dired-subtree-cycle))

(provide 'lambda-dired-subtree)

;;; lambda-dired-subtree.el ends here
