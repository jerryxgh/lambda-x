;;; lambda-eglot.el --- support elgot -*- lexical-binding: t -*-

;; Copyright (C) 2023 Guanghui Xu
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

;; speed up lsp https://github.com/blahgeek/emacs-lsp-booster
(use-package eglot-booster
        :after eglot
        :config	(eglot-booster-mode))

;; Optional: install eglot-format-buffer as a save hook.
;; The depth of -10 places this before eglot's willSave notification,
;; so that that notification reports the actual contents that will be saved.
(defun eglot-format-buffer-on-save ()
  (add-hook 'before-save-hook #'eglot-format-buffer -11 t)
  (add-hook 'before-save-hook #'eglot-code-action-organize-imports -10 t))

(use-package eglot
  :ensure
  :custom
  (eldoc-echo-area-use-multiline-p 'truncate-sym-name-if-fit) ; auto show doc info in minibuffer
  :bind (:map eglot-mode-map
              ("C-c e a" . eglot-code-actions)
              ("C-c e f" . eglot-format)
              ("C-c e h" . eldoc)
              ("C-c e i" . eglot-find-implementation)
              ("C-c e o" . eglot-code-action-organize-imports)
              ("C-c e r" . eglot-rename)
              ("C-c e F" . eglot-format-buffer)
              ("C-c e R" . eglot-reconnect))

  :config
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
  (add-hook 'c-mode-hook 'eglot-ensure)
  (add-hook 'c++-mode-hook 'eglot-ensure)
  (add-hook 'go-mode-hook 'eglot-ensure)
  (add-hook 'go-mode-hook #'eglot-format-buffer-on-save)
  (add-hook 'python-ts-mode-hook 'eglot-ensure) ; python use python-ts-mode instead of python-mode
  )

(use-package eglot-java
  :ensure t
  :config
  (add-hook 'java-mode-hook 'eglot-java-mode))

(use-package breadcrumb
  :ensure
  ;; :custom
  ;; ()
  :config
  (breadcrumb-mode t))

(provide 'lambda-eglot)

;;; lambda-eglot.el ends here
