;;; lambda-emacs-lisp.el --- emacs lisp
;; Time-stamp: <2014-04-13 11:12:29 Jerry Xu>
;;; Commentary:

;;; Code:

(require 'lambda-core)

;; eldoc --- show emacs lisp doc in the minibuffer -----------------------------
(eldoc-mode t)
(diminish 'eldoc-mode)


(provide 'lambda-emacs-lisp)

;;; lambda-emacs-lisp.el ends here
