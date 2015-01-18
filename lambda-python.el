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
                                    "python-environments"
                                    lambda-savefile-dir))


(provide 'lambda-python)

;;; lambda-python.el ends here
