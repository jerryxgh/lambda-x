;;; lambda-mac.el --- mac specific settings -*- lexical-binding: t -*-

;; Time-stamp: <2023-02-08 22:17:09 bytedance>


;;; Commentary:
;; Settings for mac os only.

;;; Code:

(when (eq system-type 'darwin)
  ;; use command as control
  (setq ns-command-modifier 'control)
  )

(provide 'lambda-mac)

;;; lambda-mac.el ends here
