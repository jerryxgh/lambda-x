;;; lambda-json.el --- json

;;; Commentary:

;;; Code:

(require 'lambda-core)
(require 'json)
(require 'treesit)

;; json-mode -------------------------------------------------------------------
(use-package json-mode
  :ensure t
  :custom
  (json-mode-indent-level 4)
  ;; :config
  )

(setq json-encoding-default-indentation "  ")

(defun lambda-json-select-mode ()
  "Use Tree-sitter for JSON when its grammar is ready."
  (if (treesit-ready-p 'json t)
      (json-ts-mode)
    (json-mode)))

;; Keep JSON associations here so later modules cannot bypass the fallback.
(add-to-list 'auto-mode-alist '("\\.jsonl?\\'" . lambda-json-select-mode))
(add-to-list 'auto-mode-alist
             '("\\(?:\\`\\|/\\)mongod[^/]*\\.log\\'" . lambda-json-select-mode))

(use-package structured-log-mode
  :vc (:url "https://github.com/lgfang/structured-log-mode" :rev :newest)
  :commands structured-log-mode)

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
