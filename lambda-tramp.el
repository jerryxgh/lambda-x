;;; lambda-tramp.el --- Remote file configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Keep remote paths and session caches independent of remote locales.

;;; Code:

(require 'lambda-core)
(require 'tramp)
(require 'tramp-cache)

(setq tramp-default-method "sshx"
      tramp-auto-save-directory lambda-auto-save-files-dir
      tramp-persistency-file-name (expand-file-name "tramp" lambda-auto-save-dir))

;; Use each host's normal environment instead of forcing a Chinese locale.
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)
(tramp-set-completion-function
 "sshx" '((tramp-parse-sconfig "/etc/ssh/ssh_config")
          (tramp-parse-sconfig "~/.ssh/config")))

(provide 'lambda-tramp)
;;; lambda-tramp.el ends here
