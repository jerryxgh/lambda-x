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
(require 'lambda-eglot)
(require 'lambda-treesit)

;; Use trae-gopls instead of the default gopls server for every Go mode.
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((go-mode go-dot-mod-mode go-dot-work-mode
                          go-ts-mode go-mod-ts-mode go-work-ts-mode)
                 "trae-gopls")))

;; https://github.com/dominikh/go-mode.el
(use-package go-mode
  :ensure
  :config
  ;; enable static check
  (setq-default eglot-workspace-configuration
                '((:gopls .
                          ((staticcheck . t)))))

  (add-hook 'go-ts-mode-hook
            (lambda ()
              (setq tab-width 4)
              (setq go-ts-mode-indent-offset tab-width)
              (setq go-mode-indent-offset tab-width)
              (setq-default tab-width 4)
              (setq-default go-ts-mode-indent-offset tab-width)
              (setq-default go-mode-indent-offset tab-width)

              (eglot-ensure)

              (add-hook 'before-save-hook
                        (lambda ()
                          ;; Format the buffer and organize imports before saving.
                          (eglot-format-buffer)
                          (call-interactively 'eglot-code-action-organize-imports))
                        nil t)))

  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-env "GOPATH")))

(require 'go-template-mode)

(provide 'lambda-golang)

;;; lambda-golang.el ends here
