;;; lambda-company.el --- company config -*- lexical-binding: t -*-

;; This file is not part of GNU Emacs.

;;; Commentary:

;; company config, http://company-mode.github.io/

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'lambda-company)

;;; Change Log:

;; Version $(3) 2021-10-30 GuanghuiXu
;;   - Initial release

;;; Code:

(require 'lambda-core)

(require 'dabbrev)
(use-package company
  :ensure t
  :pin melpa

  :diminish company-mode

  :custom
  ;; (company-tooltip-align-annotations t)
  ;; (company-require-match nil)
  (company-global-modes t)
  ;; Trigger completion immediately.
  ;; (company-idle-delay (lambda () (if (company-in-string-or-comment) nil 0.3)))
  (company-minimum-prefix-length 1)
  ;; (company-tooltip-minimum 10)
  ;; (company-frontends
  ;;  '(company-preview-common-frontend
  ;;    company-pseudo-tooltip-frontend
  ;;    company-echo-metadata-frontend))
  (company-selection-wrap-around t)

  ;; dabbrev configuration
  (dabbrev-case-fold-search nil)
  (company-dabbrev-downcase nil)

  :config

  (add-hook 'after-init-hook 'global-company-mode)

  (define-key company-mode-map (kbd "M-/") 'company-complete)
  (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "<backtab>") 'company-select-previous)
  (define-key company-active-map (kbd "C-n") 'company-select-next-or-abort)
  (define-key company-active-map (kbd "C-p") 'company-select-previous-or-abort)

  (setq
   company-backends
   '((company-capf
      company-dabbrev-code
      company-yasnippet
      company-keywords)
     company-files
     company-dabbrev))
  ;; (setq company-show-quick-access 'right)

  ;; (setq company-transformers
  ;;       ;; '(company-sort-by-backend-importance)
  ;;       ;; '(company-sort-prefer-same-case-prefix)
  ;;       '(lambda-company-sort-by-prefix-and-backend-importance))
  )

(use-package company-prescient
  :ensure t
  :config
  (company-prescient-mode 1))

(provide 'lambda-company)

;;; lambda-company.el ends here
