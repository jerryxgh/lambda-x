;;; lambda-gnus.el --- for email about settings

;; Time-stamp: <2016-05-19 13:42:38 Guanghui Xu>

;;; Commentary:

;;; Code:

(require 'lambda-core)
(require 'gnus-start)

(setq gnus-init-file (expand-file-name "gnus.el" lambda-auto-save-dir))

(provide 'lambda-gnus)

;;; lambda-gnus.el ends here
