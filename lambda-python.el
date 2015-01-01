;;; lambda-python.el --- Python

;;; Commentary:

;;; Code:

(require 'lambda-core)
;; jedi.el ---------------------------------------------------------------------
(lambda-package-ensure-install 'jedi)
;; Standard Jedi.el setting
(add-hook 'python-mode-hook '(lambda ()
                               (jedi:setup)
                               (fci-mode -1)))
(setq jedi:complete-on-dot t
      jedi:tooltip-method t
	  python-environment-directory (expand-file-name
									"auto-save-list/python-environments"
									user-emacs-directory))


(provide 'lambda-python)

;;; lambda-python.el ends here
