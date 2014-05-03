;;; lambda-python.el --- Python

;;; Commentary:

;;; Code:

(require 'lambda-core)
(lambda-package-ensure-install 'elpy)
(setq elpy-rpc-backend "jedi")
(setq skip-initialize-variables t)

(require 'elpy)
(elpy-enable)
(elpy-clean-modeline)

(when (executable-find "ipython")
  (elpy-use-ipython))

(setq elpy-default-minor-modes
	  (remove 'flymake-mode
			  elpy-default-minor-modes))


(provide 'lambda-python)

;;; lambda-python.el ends here
