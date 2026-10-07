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
(require 'lambda-eglot)
(require 'lambda-treesit)

(defun lambda-golang-server-command (&optional _interactive)
  "Choose a Go server using LAMBDA_GOPLS or the available executables."
  (list (or (getenv "LAMBDA_GOPLS")
            (executable-find "trae-gopls")
            (executable-find "gopls")
            "gopls")))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((go-mode go-dot-mod-mode go-dot-work-mode
                          go-ts-mode go-mod-ts-mode go-work-ts-mode)
                 . lambda-golang-server-command)))

(defun lambda--golang-eglot-format-and-organize ()
  "Format and organize imports for Go buffers when eglot is ready."
  (when (and (eglot-managed-p)
             (eglot-current-server))
    (condition-case err
        (progn
          (eglot-format-buffer)
          (eglot-code-actions (point-min) (point-max) "source.organizeImports" t))
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
         (go-ts-mode . lambda-golang-setup)))

(with-eval-after-load 'go-mode
  (define-key go-dot-mod-mode-map (kbd "M-n") #'flymake-goto-next-error)
  (define-key go-dot-mod-mode-map (kbd "M-p") #'flymake-goto-prev-error))

(require 'go-template-mode)

(provide 'lambda-golang)

;;; lambda-golang.el ends here
