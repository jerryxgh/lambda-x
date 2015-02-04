;;; lambda-json.el --- JavaScript

;;; Commentary:

;;; Code:

(require 'lambda-core)

;; json-mode -------------------------------------------------------------------
(lambda-package-ensure-install 'json-reformat)
(lambda-package-ensure-install 'json-snatcher)
(lambda-package-ensure-install 'json-mode)
(require 'json-mode)
(add-hook 'json-mode-hook #'(lambda ()
                              (hs-minor-mode 1)))

(provide 'lambda-json)

;;; lambda-json.el ends here
