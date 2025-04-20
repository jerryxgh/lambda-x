;;; lambda-tramp.el --- tramp configuration -*- lexical-binding: t -*-

;; This file is not part of GNU Emacs.

;;; Commentary:

;; For golang.

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'lambda-golang)

;;; Change Log:

;; Version $(1) 2025-04-20 GuanghuiXu
;;   - Initial release

;;; Code:

(require 'tramp)

;;; tramp
(setq tramp-default-method "sshx")
(tramp-set-completion-function "sshx"
                               '((tramp-parse-sconfig "/etc/ssh_config")
                                 (tramp-parse-sconfig "~/.ssh/config")))
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)


(provide 'lambda-tramp)

;;; lambda-tramp.el ends here
