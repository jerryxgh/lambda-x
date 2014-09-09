;;; lambda-eden.el --- JavaScript

;;; Commentary:

;;; Code:

(require 'lambda-core)

;; iedit -----------------------------------------------------------------------
(lambda-package-ensure-install 'iedit)
;; fold about things -----------------------------------------------------------
(lambda-package-ensure-install 'fold-this)

(defun view-time (time-seconds)
  "Convert TIME-SECONDS from the epoch (0:00 January 1, 1970 UTC) to time string."
  (current-time-string (seconds-to-time time-seconds)))

(provide 'lambda-eden)

;;; lambda-eden.el ends here
