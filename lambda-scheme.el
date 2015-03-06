;; lambda-scheme.el --- scheme - a member of the Lisp family of languages
;; Time-stamp: <2015-03-02 17:17:06 Jerry Xu>

;;; Commentary:
;; Scheme settings.

;;; Code:

(setq scheme-program-name "csi -:c")

(lambda-package-ensure-install 'geiser)

(provide 'lambda-scheme)

;;; lambda-scheme.el ends here
