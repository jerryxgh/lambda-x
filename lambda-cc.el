;;; lambda-cc.el --- c&c++ -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'lambda-core)
(require 'lambda-evil)

(defun lambda-cc-setup ()
  "Configure classic C-family indentation."
  (c-add-style "lambda" '("k&r" (c-offsets-alist . ((innamespace . 0)))) t)
  (add-to-list 'c-cleanup-list 'defun-close-semi)
  (setq-local tab-width 8)
  (setq-local indent-tabs-mode nil)
  (setq-local c-basic-offset 4)
  (c-toggle-electric-state 1))

(with-eval-after-load 'cc-mode
  (define-key c-mode-base-map (kbd "RET") #'c-context-line-break)
  (add-hook 'c-mode-common-hook #'lambda-cc-setup))

(defun lambda-cc-ts-setup ()
  "Configure tree-sitter C-family indentation."
  (setq-local tab-width 8)
  (setq-local indent-tabs-mode nil)
  (setq-local c-ts-mode-indent-offset 4)
  (setq-local c-ts-mode-indent-style 'k&r))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((c++-mode c-mode c++-ts-mode c-ts-mode) "clangd")))

(dolist (hook '(c-mode-hook c++-mode-hook c-ts-mode-hook c++-ts-mode-hook))
  (add-hook hook #'eglot-ensure))
(dolist (hook '(c-ts-mode-hook c++-ts-mode-hook))
  (add-hook hook #'lambda-cc-ts-setup))

;; ffap - find file at point ---------------------------------------------------
(autoload 'ffap-href-enable "ffap-href" nil t)
(autoload 'ffap-I-option-enable "ffap-I-option" nil t)
(with-eval-after-load 'ffap
  (require 'ffap-include-start)
  (require 'ffap-gcc-path)
  (ffap-href-enable)
  (ffap-I-option-enable))

(defvar gud-mode-map)
(defvar gdb-many-windows)

;; gdb configs -----------------------------------------------------------------
(with-eval-after-load 'gud
  (define-key gud-mode-map (kbd "<f5>") 'gud-step)
  (define-key gud-mode-map (kbd "<f6>") 'gud-next)
  (define-key gud-mode-map (kbd "<f7>") 'gud-up)
  (define-key gud-mode-map (kbd "<f8>") 'gud-go)
  )

(with-eval-after-load 'gdb-mi
  (setq gdb-many-windows t)
  )

;; cmake -----------------------------------------------------------------------
(use-package cmake-mode
  :ensure t)

(provide 'lambda-cc)

;;; lambda-cc.el ends here
