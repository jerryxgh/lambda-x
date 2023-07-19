;;; lambda-json.el --- json

;;; Commentary:

;;; Code:

(require 'lambda-core)

;; json-mode -------------------------------------------------------------------
(use-package json-mode
  :ensure t
  :custom
  (json-mode-indent-level 4)
  ;; :config
  )

(use-package json-reformat
  :ensure t)

(use-package json-snatcher
  :ensure t)

(provide 'lambda-json)

;;; lambda-json.el ends here
