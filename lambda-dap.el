;;; lambda-dap.el --- lambda-dap -*- lexical-binding: t -*-

;;; Commentary:

;; Emacs client/library for Debug Adapter Protocol is a wire protocol for
;; communication between client and Debug Server.  It’s similar to the LSP but
;; provides integration with debug server.

;; for go: https://emacs-lsp.github.io/dap-mode/page/configuration/#go

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'lambda-dap)

;;; Change Log:

;; Version $(3) 2022-09-22 GuanghuiXu
;;   - Initial release

;;; Code:

(use-package dap-mode
  :ensure
  ;; :custom
  ;; (dap-dlv-go-delve-path "/usr/local/bin/dlv")
  :config
  (dap-auto-configure-mode 1))

(provide 'lambda-dap)

;;; lambda-dap.el ends here
