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

(defun lambda--golang-eglot-format-and-organize ()
  "Format and organize imports for Go buffers when eglot is ready."
  (when (and (bound-and-true-p eglot-mode)
             (fboundp 'eglot-current-server)
             (eglot-current-server))
    (ignore-errors
      ;; 避免保存时显示消息
      (let ((inhibit-message t))
        (eglot-format-buffer)
        (when (fboundp 'eglot-code-action-organize-imports)
          ;; 非交互执行，避免走 minibuffer 流程
          (eglot-code-action-organize-imports))))))

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

              (add-hook 'before-save-hook #'lambda--golang-eglot-format-and-organize nil t)))

  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-env "GOPATH")))

(require 'go-template-mode)

(provide 'lambda-golang)

;;; lambda-golang.el ends here
