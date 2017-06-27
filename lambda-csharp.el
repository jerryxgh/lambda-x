;;; lambda-csharp.el --- csharp editing -*- lexical-binding: t -*-

;; Copyright (C) 2017 Guanghui Xu
;;
;; Author: Guanghui Xu gh_xu@qq.com

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'lambda-csharp)

;;; Change Log:

;; Version $(3) 2017-04-01 GuanghuiXu
;;   - Initial release

;;; Code:

(require 'lambda-core)

;; csharp-mode -----------------------------------------------------------------
(lambda-package-ensure-install 'csharp-mode)
(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(setq auto-mode-alist
      (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))

(provide 'lambda-csharp)

;;; lambda-csharp.el ends here
