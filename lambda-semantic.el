;;; lambda-semantic.el --- Common settings for semantic. -*- lexical-binding: t -*-

;; Copyright (C) 2023 Guanghui Xu
;;
;; Author: Guanghui Xu gh_xu@qq.com
;; Maintainer: Guanghui Xu gh_xu@qq.com
;; Created: 2023-06-25
;; Version: 0.01
;; Keywords: semantic
;; Homepage: not distributed yet
;; Package-Version: 0.01
;; Package-Requires: semantic
;;

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'lambda-semantic)

;;; Change Log:

;; Version $(3) 2023-06-25 GuanghuiXu
;;   - Initial release

;;; Code:

(require 'semantic)

(add-to-list 'semantic-inhibit-functions (lambda () (member major-mode '(java-mode emacs-lisp-mode))))

(provide 'lambda-semantic)

;;; lambda-semantic.el ends here
