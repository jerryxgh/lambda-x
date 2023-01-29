;;; lambda-bison.el --- flex and bison major mode -*- lexical-binding: t -*-

;; Copyright (C) 2022 Guanghui Xu
;;
;; Author: Guanghui Xu gh_xu@qq.com
;; Maintainer: Guanghui Xu gh_xu@qq.com
;; Created: 2022-09-22
;; Version: 0.01
;; Keywords: bison flex
;; Homepage: not distributed yet
;; Package-Version: 0.01
;; Package-Requires:
;;

;; This file is not part of GNU Emacs.

;;; Commentary:

;; major mode for flex and bison

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'lambda-bison)

;;; Change Log:

;; Version $(3) 2022-09-22 GuanghuiXu
;;   - Initial release

;;; Code:

(use-package bison-mode
  :ensure
  :mode (("\\.y\\'" . bison-mode)
         ("\\.yy\\'" . bison-mode)
         ("\\.l\\'" . flex-mode)
         ("\\.ll\\'" . flex-mode)
         )
  :pin melpa
  )

(provide 'lambda-bison)

;;; lambda-bison.el ends here
