;;; lambda-scala.el --- Scala

;;; Commentary:

;;; Code:

(lambda-package-ensure-install 'scala-mode2)
(lambda-package-ensure-install 'sbt-mode)
(lambda-package-ensure-install 'ensime)
(setq ensime-completion-style 'auto-complete)

(provide 'lambda-scala)

;;; lambda-scala.el ends here
