;;; lambda-json.el --- json -*- lexical-binding: t -*-

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
(add-to-list 'auto-mode-alist '("\\.json\\'" . lambda-json-select-mode))
(use-package structured-log-mode
  :vc (:url "https://github.com/lgfang/structured-log-mode" :rev :newest)
  :commands structured-log-mode)

(defun lambda-json-lines-format-buffer ()
  "Validate and compact each JSON Lines record, preserving one record per line."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (let* ((text (buffer-substring-no-properties (point-min) (point-max)))
             (lines (split-string text "\n"))
             (formatted
              (mapcar
               (lambda (line)
                 (if (string-blank-p line)
                     line
                   (json-serialize
                    (json-parse-string line :null-object :null :false-object :false)
                    :null-object :null :false-object :false)))
               lines)))
        ;; Parse every line before modifying the buffer.
        (atomic-change-group
          (delete-region (point-min) (point-max))
          (insert (string-join formatted "\n")))))))

(define-derived-mode lambda-json-lines-mode json-mode "JSON Lines"
  "Edit independent JSON records without whole-document formatting."
  (setq-local indent-line-function #'indent-relative))
(define-key lambda-json-lines-mode-map (kbd "C-c C-f")
            #'lambda-json-lines-format-buffer)
(define-key lambda-json-lines-mode-map [remap json-mode-beautify]
            #'lambda-json-lines-format-buffer)
(add-to-list 'auto-mode-alist '("\\.jsonl\\'" . lambda-json-lines-mode))
(add-to-list 'auto-mode-alist
             '("\\(?:\\`\\|/\\)mongod[^/]*\\.log\\'" . lambda-json-lines-mode))

(defun lambda--json-format ()
  "Format the active JSON region, or the whole buffer."
  (interactive)
  (if (use-region-p)
      (json-pretty-print (region-beginning) (region-end))
    (json-pretty-print-buffer)))

(with-eval-after-load 'json-ts-mode
  (define-key json-ts-mode-map (kbd "C-c C-f") #'lambda--json-format))

(provide 'lambda-json)
;;; lambda-json.el ends here
