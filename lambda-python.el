;;; lambda-python.el --- python configuration -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'lambda-eglot)

(use-package python
  :ensure nil
  :custom
  (python-indent-offset 4))

(defun lambda-python-select-mode ()
  "Use Tree-sitter for Python when its grammar is ready."
  (if (treesit-ready-p 'python t)
      (python-ts-mode)
    (python-mode)))

(add-to-list 'major-mode-remap-alist '(python-mode . lambda-python-select-mode))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode)
                 "pyright-langserver" "--stdio")))
(add-hook 'python-mode-hook 'eglot-ensure)
(add-hook 'python-ts-mode-hook 'eglot-ensure)

(provide 'lambda-python)

;;; lambda-python.el ends here
