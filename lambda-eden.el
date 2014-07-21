;;; lambda-eden.el --- JavaScript

;;; Commentary:

;;; Code:

(require 'lambda-core)

;; iedit -----------------------------------------------------------------------
(lambda-package-ensure-install 'iedit)
;; fold about things -----------------------------------------------------------
(lambda-package-ensure-install 'fold-this)
(lambda-package-ensure-install 'fold-dwim)


(provide 'lambda-eden)

;;; lambda-eden.el ends here
