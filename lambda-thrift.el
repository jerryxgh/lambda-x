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

(lambda-package-ensure-install 'thrift)
(require 'thrift)
(setq thrift-indent-level 4)
(add-hook 'thrift-mode-hook (lambda ()
                              (semantic-mode 1)
                             ))

;; use semanticdb-find-test-translate-path to debug include
(add-to-list 'load-path "/Users/bytedance/repository/public/lambda-thrift")
(require 'thrift-wy)
(require 'thrift-tags)
(add-to-list 'semantic-new-buffer-setup-functions '(thrift-mode . wisent-thrift-default-setup))
(require 'eieio-datadebug)

(provide 'lambda-thrift)

;;; lambda-thrift.el ends here
