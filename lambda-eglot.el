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

(use-package eglot
  :ensure
  :custom
  (eldoc-echo-area-use-multiline-p 'truncate-sym-name-if-fit) ; auto show doc info in minibuffer

  :config
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
  (add-hook 'c-mode-hook 'eglot-ensure)
  (add-hook 'c++-mode-hook 'eglot-ensure)
  (add-hook 'python-mode-hook 'eglot-ensure))

(use-package eglot-java
  :ensure t
  :config
  (add-hook 'java-mode-hook 'eglot-java-mode))

(provide 'lambda-eglot)

;;; lambda-eglot.el ends here
