;;; lambda-mac.el --- mac specific settings -*- lexical-binding: t -*-

;; Time-stamp: <2016-11-01 11:33:37 Guanghui Xu>


;;; Commentary:
;; Settings for mac os only.

;;; Code:

(require 'lambda-core)

(when (eq system-type 'darwin)
  ;; use command as control
  (setq ns-command-modifier 'control)
  )

(provide 'lambda-mac)

;;; lambda-mac.el ends here
