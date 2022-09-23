;;; lambda-company.el --- company config -*- lexical-binding: t -*-

;; Copyright (C) 2021 Guanghui Xu
;;
;; Author: Guanghui Xu gh_xu@qq.com
;; Maintainer: Guanghui Xu gh_xu@qq.com
;; Created: 2021-10-30
;; Version: 0.1
;; Keywords: company
;; Homepage: not distributed yet
;; Package-Version: 0.1
;; Package-Requires:
;;

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

(use-package company
  :ensure t
  :diminish company-mode
  :custom
  (company-tooltip-align-annotations t)
  (company-require-match t)
  ;; Don't use company in the following modes
  ;; (company-global-modes '(not shell-mode eaf-mode))
  (company-global-modes t)
  ;; Trigger completion immediately.
  (company-idle-delay (lambda () (if (company-in-string-or-comment) nil 0.3)))
  (company-show-quick-access 'right)
  (company-minimum-prefix-length 1)
  (company-tooltip-minimum 10)
  (company-frontends
        '(company-pseudo-tooltip-unless-just-one-frontend-with-delay
          company-preview-frontend
          company-echo-metadata-frontend
          ))
  (company-selection-wrap-around t)
  :config

  (add-hook 'after-init-hook 'global-company-mode)

  (define-key company-mode-map (kbd "M-/") 'company-complete)
  (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)

  ;; (push '(company-capf :with company-yasnippet :with company-dabbrev-code :with company-keywords :with company-gtags :with company-files) company-backends)
  (setq company-backends '((company-capf :with company-dabbrev company-yasnippet company-keywords company-gtags)))
  (setq company-transformers
        ;; '(company-sort-by-backend-importance)
        '(company-sort-prefer-same-case-prefix))
  )

(provide 'lambda-company)

;;; lambda-company.el ends here
