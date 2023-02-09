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
(require 'lambda-company)

;; integrate with helm
(use-package helm-lsp
  :ensure
  :commands helm-lsp-workspace-symbol
  :config
  (define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol)
  )

;; xref complete by helm
(use-package helm-xref
  :ensure)

;; integrate with treemacs
(if (featurep 'treemacs)
    (use-package lsp-treemacs :commands lsp-treemacs-errors-list
      :config
      (lsp-treemacs-sync-mode 1)))

;; https://github.com/dominikh/go-mode.el
(use-package go-mode
  :ensure
  :config
  (add-hook 'go-mode-hook #'lsp-deferred)
  (add-hook 'go-mode-hook #'(lambda ()
                              (setq tab-width 4)))
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-env "GOPATH"))
  )

(use-package lsp-mode
  ;; :init
  :hook (;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))

  :commands lsp
  :config
  (define-key lsp-mode-map (kbd "C-c C-l") lsp-command-map)
  (define-key lsp-mode-map (kbd "<f6>") 'lsp-rename)
  (add-hook 'lsp-mode-hook
            (lambda ()
              (setq company-backends
                    '((company-capf
                       :with lambda-company-yasnippet lambda-company-dabbrev lambda-company-keywords)))

              (when (and (featurep 'evil) (featurep 'evil-leader))
                (define-key evil-normal-state-map (kbd "g i") 'lsp-find-implementation)
                (define-key evil-normal-state-map (kbd "g r") 'xref-find-references))))

  (defun lsp-go-install-save-hooks ()
    "Set up before-save hooks to format buffer and add/delete imports.
Make sure you don't have other gofmt/goimports hooks enabled."
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))
  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
  (lsp-register-custom-settings
   '(("gopls.completeUnimported" t t)
     ("gopls.staticcheck" t t))))

(use-package lsp-ui
  :ensure
  :custom
  ;; https://emacs-lsp.github.io/lsp-mode/page/performance/
  (lsp-ui-doc-enable nil)
  (lsp-ui-imenu-enable nil)
  (lsp-ui-peek-enable nil)
  (lsp-ui-sideline-enable nil)
  (lsp-ui-sideline-show-hover nil)
  )

(provide 'lambda-golang)

;;; lambda-golang.el ends here
