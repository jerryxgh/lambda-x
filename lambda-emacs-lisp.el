;;; lambda-emacs-lisp.el --- emacs lisp
;; Time-stamp: <2015-08-23 12:42:23 Jerry Xu>
;;; Commentary:

;;; Code:

(require 'lambda-core)

;; eldoc --- show emacs lisp doc in the minibuffer -----------------------------
(add-hook 'emacs-lisp-mode-hook
          #'(lambda ()
              (eldoc-mode 1)
              (diminish 'eldoc-mode)
              (setq-default flycheck-emacs-lisp-load-path load-path)))

(lambda-package-ensure-install 'flycheck-package)
(eval-after-load 'flycheck '(flycheck-package-setup))
;; (eval-after-load 'flycheck
;;   '(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

;; elisp-slime-nav --- Make M-. and M-, work in elisp like they do in slime-----
(lambda-package-ensure-install 'elisp-slime-nav)
(require 'elisp-slime-nav) ;; optional if installed via package.el
(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'turn-on-elisp-slime-nav-mode))

;; morlock --- more font-lock keywords for elisp -------------------------------
(lambda-package-ensure-install 'morlock)
(require 'morlock)
(font-lock-add-keywords 'emacs-lisp-mode
                        morlock-el-font-lock-keywords)


(provide 'lambda-emacs-lisp)

;;; lambda-emacs-lisp.el ends here
