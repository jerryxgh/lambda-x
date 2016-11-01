;;; lambda-groovy.el --- Groovy

;;; Commentary:

;;; Code:

(require 'lambda-core)
(require 'lambda-cc)

;;; groovy =====================================================================
(lambda-package-ensure-install 'groovy-mode)
(lambda-package-ensure-install 'groovy-imports)
(with-eval-after-load 'groovy-mode
  (require 'groovy-imports)
  (define-key groovy-mode-map (kbd "M-I") 'groovy-imports-add-import))

(provide 'lambda-groovy)

;;; lambda-groovy.el ends here
