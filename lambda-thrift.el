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

;; use semanticdb-find-test-translate-path to debug include
(add-to-list 'load-path "/Users/bytedance/repository/public/lambda-thrift")
(require 'lambda-thrift-tags)

(use-package thrift
  :ensure t
  :bind (:map thrift-mode-map ("M-." . semantic-ia-fast-jump))
  :custom
  (thrift-indent-level 4)
  (thrift-mode-syntax-table lambda-thrift-syntax-table)
  :hook ((thrift-mode . (lambda ()
                          (semantic-mode 1))))
  :pin melpa)

(provide 'lambda-thrift)

;;; lambda-thrift.el ends here
