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

(provide 'lambda-thrift)

;;; lambda-thrift.el ends here
