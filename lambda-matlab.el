;;; lambda-matlab.el --- JavaScript

;;; Commentary:

;;; Code:

(require 'lambda-core)

;; matlab-mode -----------------------------------------------------------------
(lambda-package-ensure-install 'matlab-mode)
(require 'matlab)
(setq matlab-shell-command "/home/xgh/local/matlabR2013b/bin/matlab"
	  matlab-shell-command-switches '("-nodesktop -nosplash")
	  matlab-shell-enable-gud-flag nil)
(add-to-list 'matlab-mode-install-path
			 "/home/xgh/local/matlabR2013b")


(provide 'lambda-matlab)

;;; lambda-matlab.el ends here
