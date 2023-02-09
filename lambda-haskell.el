;;; lambda-haskell.el --- Haskell

;;; Commentary:

;;; Code:

(require 'lambda-core)

(use-package haskell-mode
  :ensure t)
(use-package flycheck-haskell
  :ensure t
  :hook ((flycheck-mode . flycheck-haskell-setup)))

(provide 'lambda-haskell)

;;; lambda-haskell.el ends here
