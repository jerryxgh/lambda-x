;;; lambda-golang.el --- for go programming -*- lexical-binding: t -*-

;; Copyright (C) 2021 Guanghui Xu
;;
;; Author: Guanghui Xu gh_xu@qq.com
;; Maintainer: Guanghui Xu gh_xu@qq.com
;; Created: 2021-10-21
;; Version: 0.1
;; Keywords: go,golang
;; Homepage: not distributed yet
;; Package-Version: 0.1
;; Package-Requires: go-mode
;;

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'lambda-go)

;;; Change Log:

;; Version $(3) 2021-10-21 GuanghuiXu
;;   - Initial release

;;; Code:

(require 'lambda-core)
(require 'lambda-cc)


;; https://github.com/dominikh/go-mode.el
(lambda-package-ensure-install 'go-mode)
(lambda-package-ensure-install 'lsp-mode)
(lambda-package-ensure-install 'lsp-ui)

(require 'go-mode)

;;; syntax chech for golang
(require 'lsp-ui)
;; https://emacs-lsp.github.io/lsp-mode/page/performance/
(require 'lsp-mode)
(setq lsp-ui-doc-enable nil
      lsp-ui-imenu-enable nil
      lsp-ui-peek-enable nil
      lsp-ui-sideline-enable t
      lsp-ui-sideline-show-hover t
      )

(add-hook 'go-mode-hook #'lsp-deferred)
(add-hook 'go-mode-hook #'(lambda ()
                            (setq tab-width 4)))

(add-hook 'lsp-mode-hook
          #'(lambda ()
              (when (and (featurep 'evil) (featurep 'evil-leader))
                (define-key evil-normal-state-map (kbd "g i") 'lsp-find-implementation)
                (define-key evil-normal-state-map (kbd "g r") 'xref-find-references))))

(defun lsp-go-install-save-hooks ()
  "Set up before-save hooks to format buffer and add/delete imports.
Make sure you don't have other gofmt/goimports hooks enabled."
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "GOPATH"))


;; integrate with treemacs
(lambda-package-ensure-install 'lsp-treemacs)
(lsp-treemacs-sync-mode 1)

;; integrate with helm
(lambda-package-ensure-install 'helm-lsp)
(define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol)
;; xref complete by helm
(lambda-package-ensure-install 'helm-xref)

(provide 'lambda-golang)

;;; lambda-golang.el ends here
