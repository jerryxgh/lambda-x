;;; lambda-json.el --- json

;;; Commentary:

;;; Code:

(require 'lambda-core)
(require 'json)

;; json-mode -------------------------------------------------------------------
(use-package json-mode
  :ensure t
  :custom
  (json-mode-indent-level 4)
  ;; :config
  )

(setq json-encoding-default-indentation "  ")

(defun lambda--json-format ()
  "Format the active JSON region, or the whole buffer."
  (interactive)
  (if (use-region-p)
      (json-pretty-print (region-beginning) (region-end))
    (json-pretty-print-buffer)))

(with-eval-after-load 'json-ts-mode
  (define-key json-ts-mode-map
              (kbd "C-c C-f")
              #'lambda--json-format))

(provide 'lambda-json)

;;; lambda-json.el ends here
