;;; lambda-thrift.el --- support for thrift -*- lexical-binding: t -*-

;; Copyright (C) 2021 Guanghui Xu

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
(require 'lambda-evil)
(require 'lambda-company)

(use-package semantic-thrift
  :ensure t
  :config
  ;; enable semantic-mode when open thrift file
  (add-hook 'thrift-mode-hook (lambda ()
                                (semantic-mode 1)
                                (if (featurep 'company)
                                    (setq company-backends '((lambda-company-yasnippet lambda-company-dabbrev-code lambda-company-dabbrev lambda-company-keywords))))))
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
