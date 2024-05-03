;;; lambda-python.el --- python configuration -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'lambda-eglot)

(use-package python
  :custom
  (python-indent-offset 4))

(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))

(add-to-list 'eglot-server-programs '(python-mode . ("pyright-langserver" "--stdio")))
(add-hook 'python-ts-mode-hook 'eglot-ensure) ; python use python-ts-mode instead of python-mode

(provide 'lambda-python)

;;; lambda-python.el ends here
