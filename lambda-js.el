;;; lambda-js.el --- JavaScript

;;; Commentary:

;;; Code:

;; js3-mode --------------------------------------------------------------------
(lambda-package-ensure-install 'js3-mode)
(require 'js3-mode)
(add-hook 'js3-mode-hook (lambda ()
						   "Set default values of the variables."
						   (setq js3-auto-indent-p t
								 js3-curly-indent-offset 0
								 js3-enter-indents-newline t
								 js3-expr-indent-offset 2
								 js3-indent-on-enter-key t
								 js3-lazy-commas t
								 js3-lazy-dots t
								 js3-lazy-operators t
								 js3-paren-indent-offset 2
								 js3-square-indent-offset 4)))

(add-to-list 'auto-mode-alist '("\\.avpr" . js3-mode))
(add-to-list 'auto-mode-alist '("\\.avsc" . js3-mode))

;; nodejs-repl -----------------------------------------------------------------
(lambda-package-ensure-install 'nodejs-repl)
(require 'nodejs-repl)

(provide 'lambda-js)

;;; lambda-js.el ends here
