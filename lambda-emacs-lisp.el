;;; lambda-emacs-lisp.el --- emacs lisp
;; Time-stamp: <2014-08-07 21:57:22 Jerry Xu>
;;; Commentary:

;;; Code:

(require 'lambda-core)

;; eldoc --- show emacs lisp doc in the minibuffer -----------------------------
(eldoc-mode t)
(diminish 'eldoc-mode)
(add-hook 'emacs-lisp-mode-hook
          #'(lambda ()
              (setq-default flycheck-emacs-lisp-load-path load-path)))

(provide 'lambda-emacs-lisp)

;;; lambda-emacs-lisp.el ends here
