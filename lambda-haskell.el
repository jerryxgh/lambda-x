;;; lambda-haskell.el --- Haskell

;;; Commentary:

;;; Code:

(require 'lambda-core)

(lambda-package-ensure-install 'haskell-mode)
(lambda-package-ensure-install 'flycheck-haskell)
(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup)

(provide 'lambda-haskell)

;;; lambda-haskell.el ends here
