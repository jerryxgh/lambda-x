;;; lambda-eglot.el --- support elgot -*- lexical-binding: t -*-

;; Copyright (C) 2023 Guanghui Xu
;;
;; Author: Guanghui Xu gh_xu@qq.com
;; Maintainer: Guanghui Xu gh_xu@qq.com
;; Created: 2023-05-30
;; Version: 0.01
;; Keywords:
;; Homepage: not distributed yet
;; Package-Version: 0.01
;; Package-Requires:
;;

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Support for eglot

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'lambda-eglot)

;;; Change Log:

;; Version $(3) 2023-05-30 GuanghuiXu
;;   - Initial release

;;; Code:

(require 'lambda-core)
(require 'lambda-cc)
(require 'lambda-company)

(use-package eglot
  :ensure
  :config

  (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
  (add-hook 'c-mode-hook (lambda ()
                           (eglot-ensure)
                           (if flycheck-mode
                               (flycheck-mode nil))))
  (add-hook 'c++-mode-hook (lambda ()
                           (eglot-ensure)
                           (if flycheck-mode
                               (flycheck-mode nil))))
  )

(provide 'lambda-eglot)

;;; lambda-eglot.el ends here
