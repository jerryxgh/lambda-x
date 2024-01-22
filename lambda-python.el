;;; lambda-python.el --- python configuration -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package python
  :custom
  (python-indent-offset 4))

(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))

(provide 'lambda-python)

;;; lambda-python.el ends here
