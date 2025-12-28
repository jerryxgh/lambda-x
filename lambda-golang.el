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

;; https://github.com/dominikh/go-mode.el
(use-package go-mode
  :ensure
  :config
  (add-hook 'go-mode-hook
            (lambda ()
              (setq tab-width 4)

              (eglot-ensure)
              (add-hook 'before-save-hook
                        (lambda ()
                          ;; auto format and organize imports when save buffer
                          (eglot-format-buffer)
                          (call-interactively 'eglot-code-action-organize-imports))
                        nil t))
            ;; enable static check
            (setq-default eglot-workspace-configuration
                          '((:gopls .
                                    ((staticcheck . t))))))

  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-env "GOPATH")))

(require 'go-template-mode)

(provide 'lambda-golang)

;;; lambda-golang.el ends here
