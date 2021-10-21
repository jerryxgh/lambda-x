;;; lambda-go.el --- for go programming -*- lexical-binding: t -*-

;; Copyright (C) 2021 Guanghui Xu
;;
;; Author: Guanghui Xu gh_xu@qq.com
;; Maintainer: Guanghui Xu gh_xu@qq.com
;; Created: 2021-10-21
;; Version: 0.1
;; Keywords: go
;; Homepage: not distributed yet
;; Package-Version: 0.1
;; Package-Requires: go-mode
;;

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'lambda-go)

;;; Change Log:

;; Version $(3) 2021-10-21 GuanghuiXu
;;   - Initial release

;;; Code:

(require 'lambda-core)
(require 'lambda-cc)


;; https://github.com/dominikh/go-mode.el
(lambda-package-ensure-install 'go-mode)
(lambda-package-ensure-install 'lsp-mode)

(require 'go-mode)
(require 'lsp-mode)

(add-hook 'go-mode-hook #'lsp-deferred)
(defun lsp-go-install-save-hooks ()
  "Set up before-save hooks to format buffer and add/delete imports.
Make sure you don't have other gofmt/goimports hooks enabled."
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

(provide 'lambda-go)

;;; lambda-go.el ends here
