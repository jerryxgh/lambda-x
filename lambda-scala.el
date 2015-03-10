;;; lambda-scala.el --- Scala

;;; Commentary:

;;; Code:

(require 'lambda-core)

;; scala-mode2 -----------------------------------------------------------------
(lambda-package-ensure-install 'scala-mode2)

;; ensime ----------------------------------------------------------------------
(lambda-package-ensure-install 'ensime)
;; This step causes the ensime-mode to be started whenever
;; scala-mode is started for a buffer. You may have to customize this step
;; if you're not using the standard scala mode.
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
(setq ensime-completion-style 'auto-complete)

;; sbt-mode --------------------------------------------------------------------
(lambda-package-ensure-install 'sbt-mode)

(provide 'lambda-scala)

;;; lambda-scala.el ends here
