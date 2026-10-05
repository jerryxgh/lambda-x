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
  (when (and (eglot-managed-p)
             (eglot-current-server))
    (condition-case err
        (progn
          (eglot-format-buffer)
          (eglot-code-action-organize-imports))
      ;; Save the file even if the server cannot handle either operation.
      (error (message "Go save processing failed: %s"
                      (error-message-string err))))))

(defun lambda-golang-setup ()
  "Configure indentation, Eglot and save processing for Go source buffers."
  (setq-local tab-width 4)
  (when (derived-mode-p 'go-ts-mode)
    (setq-local go-ts-mode-indent-offset 4))
  (setq-local eglot-workspace-configuration
              '((:gopls . ((staticcheck . t)))))
  (add-hook 'before-save-hook #'lambda--golang-eglot-format-and-organize nil t)
  (eglot-ensure))

;; https://github.com/dominikh/go-mode.el
(use-package go-mode
  :ensure t
  :hook ((go-mode . lambda-golang-setup)
         (go-ts-mode . lambda-golang-setup))
  :config

  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-env "GOPATH")))

(require 'go-template-mode)

(provide 'lambda-golang)

;;; lambda-golang.el ends here
