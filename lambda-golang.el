;;; lambda-golang.el --- for go programming -*- lexical-binding: t -*-

;; This file is not part of GNU Emacs.

;;; Commentary:

;; For golang.

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'lambda-golang)

;;; Change Log:

;; Version $(3) 2021-10-21 GuanghuiXu
;;   - Initial release

;;; Code:

(require 'lambda-core)
(require 'lambda-cc)
(require 'lambda-company)
(require 'lambda-eglot)

;; https://github.com/dominikh/go-mode.el
(use-package go-mode
  :ensure
  :config
  (add-hook 'go-mode-hook (lambda ()
                            (setq tab-width 4)
                            (eglot-ensure)))
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-env "GOPATH")))

;; (use-package lsp-mode
;;   ;; :init
;;   :hook (;; if you want which-key integration
;;          (lsp-mode . lsp-enable-which-key-integration))

;;   :commands lsp
;;   :config
;;   (define-key lsp-mode-map (kbd "C-c C-l") lsp-command-map)
;;   (define-key lsp-mode-map (kbd "<f6>") 'lsp-rename)
;;   (add-hook 'lsp-mode-hook
;;             (lambda ()
;;               (setq company-backends
;;                     '((company-capf
;;                        :with lambda-company-yasnippet lambda-company-dabbrev-code lambda-company-dabbrev lambda-company-keywords)))

;;               (when (and (featurep 'evil) (featurep 'evil-leader))
;;                 (define-key evil-normal-state-map (kbd "g i") 'lsp-find-implementation)
;;                 (define-key evil-normal-state-map (kbd "g r") 'xref-find-references))))

;;   (defun lsp-go-install-save-hooks ()
;;     "Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled."
;;     (add-hook 'before-save-hook #'lsp-format-buffer t t)
;;     (add-hook 'before-save-hook #'lsp-organize-imports t t))
;;   (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
;;   (lsp-register-custom-settings
;;    '(("gopls.completeUnimported" t t)
;;      ("gopls.staticcheck" t t))))

;; (use-package lsp-ui
;;   :ensure
;;   :custom
;;   ;; https://emacs-lsp.github.io/lsp-mode/page/performance/
;;   (lsp-ui-doc-enable nil)
;;   (lsp-ui-imenu-enable nil)
;;   (lsp-ui-peek-enable nil)
;;   (lsp-ui-sideline-enable nil)
;;   (lsp-ui-sideline-show-hover nil)
;;   )

;; ;; integrate with treemacs
;; (if (featurep 'treemacs)
;;     (use-package lsp-treemacs :commands lsp-treemacs-errors-list
;;       :config
;;       (lsp-treemacs-sync-mode 1)))

(provide 'lambda-golang)

;;; lambda-golang.el ends here
