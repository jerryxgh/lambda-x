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

(defun company-mode/backend-with-same-mode (backend)
  "Add company-same-mode-buffers for BACKEND."
  (if (and (listp backend) (member 'company-same-mode-buffers backend))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-same-mode-buffers))
    ))

(use-package company
  :ensure t
  :diminish company-mode
  :hook ((prog-mode LaTeX-mode latex-mode ess-r-mode) . company-mode)
  ;; :bind
  ;; :map (company-active-map
  ;;       ([tab] . smarter-tab-to-complete)
  ;;       ("TAB" . smarter-tab-to-complete))
  :custom
  (company-tooltip-align-annotations t)
  (company-require-match 'never)
  ;; Don't use company in the following modes
  (company-global-modes '(not shell-mode eaf-mode))
  ;; Trigger completion immediately.
  (company-idle-delay 0.1)
  (company-show-quick-access 'right)
  (company-tooltip-minimum 10)
  :config


  (setq company-frontends
        '(company-pseudo-tooltip-unless-just-one-frontend-with-delay
          company-preview-frontend
          company-echo-metadata-frontend
          ))
  (defun smarter-tab-to-complete ()
    "Try to `org-cycle', `yas-expand', and `yas-next-field' at current cursor position.

If all failed, try to complete the common part with `company-complete-common'"
    (interactive)
    (when yas-minor-mode
      (let ((old-point (point))
            (old-tick (buffer-chars-modified-tick))
            (func-list
             (if (equal major-mode 'org-mode) '(org-cycle yas-expand yas-next-field)
               '(yas-expand yas-next-field))))
        (catch 'func-suceed
          (dolist (func func-list)
            (ignore-errors (call-interactively func))
            (unless (and (eq old-point (point))
                         (eq old-tick (buffer-chars-modified-tick)))
              (throw 'func-suceed t)))
          (company-complete-common)))))

  (add-hook 'after-init-hook 'global-company-mode)

  (define-key company-mode-map (kbd "M-/") 'company-complete)
  (define-key company-active-map (kbd "<tab>") 'company-complete)
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)

  ;; (push 'company-elisp company-backends)
  ;; (push 'company-dabbrev company-backends)

  ;; (require 'company-same-mode-buffers)
  ;; (company-same-mode-buffers-initialize)

  ;; (setq company-backends (mapcar #'company-mode/backend-with-same-mode company-backends))
  ;; (push 'company-same-mode-buffers company-backends)

  ;; (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))
  ;; (push 'company-yasnippet company-backends)
  )

(provide 'lambda-company)

;;; lambda-company.el ends here
