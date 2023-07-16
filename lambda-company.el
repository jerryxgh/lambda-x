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

(defun lambda-company-sort-by-prefix-and-backend-importance (candidates)
  "Prefer CANDIDATES with the exact same prefix.
If a backend returns case insensitive matches, candidates with the an exact
prefix match (same case) will be prioritized."
  (cl-loop for candidate in candidates
           if (string-prefix-p company-prefix candidate)
           collect candidate into same-case
           else collect candidate into other-case
           finally return (append (company-sort-by-backend-importance same-case) (company-sort-by-backend-importance other-case))))

(defun lambda-company-dabbrev (command &optional arg &rest ignored)
  "If preceding char is dot(.), skip completion.
All args are passed directory, including COMMAND ARG and IGNORED."
  (if (eq (preceding-char) ?\.)
      nil
      (company-dabbrev command arg ignored)))

(defun lambda-company-dabbrev-code (command &optional arg &rest ignored)
  "If preceding char is dot(.), skip completion.
All args are passed directory, including COMMAND ARG and IGNORED."
  (if (eq (preceding-char) ?\.)
      nil
      (company-dabbrev-code command arg ignored)))

(defun lambda-company-yasnippet (command &optional arg &rest ignored)
  "If preceding char is dot(.), skip completion.
All args are passed directory, including COMMAND ARG and IGNORED."
  (if (eq (preceding-char) ?\.)
      nil
      (company-yasnippet command arg ignored)))

(defun lambda-company-keywords (command &optional arg &rest ignored)
  "If preceding char is dot(.), skip completion.
All args are passed directory, including COMMAND ARG and IGNORED."
  (if (eq (preceding-char) ?\.)
      nil
      (company-keywords command arg ignored)))

(require 'dabbrev)
(use-package company
  :ensure t
  :pin melpa

  :diminish company-mode

  :custom
  (company-tooltip-align-annotations t)
  (company-require-match nil)
  ;; Don't use company in the following modes
  ;; (company-global-modes '(not shell-mode eaf-mode))
  (company-global-modes t)
  ;; Trigger completion immediately.
  (company-idle-delay (lambda () (if (company-in-string-or-comment) nil 0.3)))
  (company-minimum-prefix-length 1)
  (company-tooltip-minimum 10)
  ;; (company-frontends
  ;;  '(company-pseudo-tooltip-unless-just-one-frontend-with-delay
  ;;    company-preview-frontend
  ;;    company-echo-metadata-frontend
  ;;    ))
  (company-selection-wrap-around t)

  ;; dabbrev configuration
  (dabbrev-case-fold-search nil)
  (dabbrev-upcase-means-case-search t)
  (company-box-doc-enable nil)
  (company-dabbrev-downcase nil)

  :config

  (add-hook 'after-init-hook 'global-company-mode)

  (define-key company-mode-map (kbd "M-/") 'company-complete)
  (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "<backtab>") 'company-select-previous)
  ;; (define-key company-active-map (kbd "M-n") nil)
  ;; (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)

  (setq company-backends '((company-capf :with lambda-company-yasnippet lambda-company-dabbrev-code lambda-company-dabbrev lambda-company-keywords))
        company-show-quick-access 'right)

  (make-variable-buffer-local 'company-backends)
  (add-hook 'makefile-mode-hook
            (lambda ()
              (setq company-backends
                    '((lambda-company-yasnippet lambda-company-dabbrev-code lambda-company-dabbrev lambda-company-keywords)))))

  (setq company-transformers
        ;; '(company-sort-by-backend-importance)
        ;; '(company-sort-prefer-same-case-prefix)
        '(lambda-company-sort-by-prefix-and-backend-importance)))

;; company-box
(use-package company-box
  :after company
  :ensure t
  :diminish company-box-mode
  :hook (company-mode . company-box-mode)
  :config
  (add-to-list 'company-box-frame-parameters '(line-spacing . 3))
  (add-to-list 'company-box-frame-parameters '(line-height . 18))
  (add-to-list 'company-box-frame-parameters
               (cond ((eq system-type 'windows-nt)
                      '(font . "Consolas-11"))
                     ((eq system-type 'gnu/linux)
                      '(font . "Source Code Pro-11"))
                     ((eq system-type 'darwin)
                      '(font . "menlo-14")))))

(provide 'lambda-company)

;;; lambda-company.el ends here
