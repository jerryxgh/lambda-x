;;; lambda-emacs-lisp.el --- emacs lisp
;; Time-stamp: <2015-07-08 10:32:00 Jerry Xu>
;;; Commentary:

;;; Code:

(require 'lambda-core)

;; eldoc --- show emacs lisp doc in the minibuffer -----------------------------
(diminish 'eldoc-mode)
(add-hook 'emacs-lisp-mode-hook
          #'(lambda ()
              (eldoc-mode 1)
              (setq-default flycheck-emacs-lisp-load-path load-path)))

(lambda-package-ensure-install 'flycheck-package)
(eval-after-load 'flycheck '(flycheck-package-setup))
;; (eval-after-load 'flycheck
;;   '(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(provide 'lambda-emacs-lisp)

;;; lambda-emacs-lisp.el ends here
