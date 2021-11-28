;;; lambda-company.el --- company config -*- lexical-binding: t -*-

;; Copyright (C) 2021 Guanghui Xu
;;
;; Author: Guanghui Xu gh_xu@qq.com
;; Maintainer: Guanghui Xu gh_xu@qq.com
;; Created: 2021-10-30
;; Version: 0.1
;; Keywords: company
;; Homepage: not distributed yet
;; Package-Version: 0.1
;; Package-Requires:
;;

;; This file is not part of GNU Emacs.

;;; Commentary:

;; company config, http://company-mode.github.io/

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'lambda-company)

;;; Change Log:

;; Version $(3) 2021-10-30 GuanghuiXu
;;   - Initial release

;;; Code:

(require 'lambda-core)

(lambda-package-ensure-install 'company)
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(define-key company-mode-map (kbd "M-/") 'company-complete)
(define-key company-active-map (kbd "<tab>") 'company-complete)

(push 'company-elisp company-backends)
(push 'company-dabbrev company-backends)

;; Add yasnippet support for all company backends
;; https://github.com/syl20bnr/spacemacs/pull/179
(defvar company-mode/enable-yas t
  "Enable yasnippet for all backends.")

(defun company-mode/backend-with-yas (backend)
  "Add yasnippet for BACKEND."
  (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))

    ))

(setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))
(push 'company-yasnippet company-backends)

(require 'company-same-mode-buffers)
(company-same-mode-buffers-initialize)

(defun company-mode/backend-with-same-mode (backend)
  "Add company-same-mode-buffers for BACKEND."
  (if (and (listp backend) (member 'company-same-mode-buffers backend))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-same-mode-buffers))
    ))
(setq company-backends (mapcar #'company-mode/backend-with-same-mode company-backends))
(push 'company-same-mode-buffers company-backends)


(lambda-package-ensure-install 'company-fuzzy)
;; (global-company-fuzzy-mode 1)

(lambda-package-ensure-install 'company-wordfreq)

(provide 'lambda-company)

;;; lambda-company.el ends here
