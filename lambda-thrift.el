;;; lambda-thrift.el --- support for thrift -*- lexical-binding: t -*-

;; Copyright (C) 2021 Guanghui Xu
;;
;; Author: Guanghui Xu gh_xu@qq.com
;; Maintainer: Guanghui Xu gh_xu@qq.com
;; Created: 2021-11-03
;; Version:
;; Keywords:
;; Homepage: not distributed yet
;; Package-Version:
;; Package-Requires:
;;

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'lambda-thrift)

;;; Change Log:

;; Version $(3) 2021-11-03 GuanghuiXu
;;   - Initial release

;;; Code:

(require 'lambda-core)
(require 'cc-mode)
(require 'eieio-datadebug)
(require 'lambda-evil)

;; (use-package thrift
;;   :ensure t)

;; (add-to-list 'load-path "/Users/bytedance/repository/public/semantic-thrift")
;; ;; (add-to-list 'load-path "/Users/hudandan/repository/lambda-thrift")

;; (require 'semantic-thrift)
;; (with-eval-after-load 'semantic-thrift
;;   (add-hook 'thrift-mode-hook (lambda ()
;;                                 (semantic-mode 1)
;;                                 (if (featurep 'company)
;;                                     (setq company-backends '((lambda-company-yasnippet lambda-company-dabbrev-code lambda-company-dabbrev lambda-company-keywords))))))
;;   (if (bound-and-true-p evil-mode)
;;       ;; support evil-jump
;;       (define-key thrift-mode-map (kbd "M-.") 'evil-goto-definition)
;;     (define-key thrift-mode-map (kbd "M-.") 'semantic-ia-fast-jump))
;;   (setq thrift-mode-syntax-table semantic-thrift-syntax-table
;;         thrift-indent-level 4)
;;   )

(use-package semantic-thrift
  :ensure t
  :config
  ;; enable semantic-mode when open thrift file
  (add-hook 'thrift-mode-hook (lambda () (semantic-mode 1)))
  ;; only thrift-mode use semantic, since at present most language use lsp instead of semantic
  (add-to-list 'semantic-inhibit-functions (lambda () (not (member major-mode '(thrift-mode)))))

  (if (bound-and-true-p evil-mode)
      ;; support evil-jump
      (define-key thrift-mode-map (kbd "M-.") 'evil-goto-definition)
    (define-key thrift-mode-map (kbd "M-.") 'semantic-ia-fast-jump))

  ;; thrift-mode syntax-table is too weak, it cann't process <> correctly
  (setq thrift-mode-syntax-table semantic-thrift-syntax-table))

(provide 'lambda-thrift)

;;; lambda-thrift.el ends here
