;;; lambda-emacs-lisp.el --- emacs lisp

;;; Commentary:

;;; Code:

(require 'lambda-core)

;; eldoc --- show emacs lisp doc in the minibuffer -----------------------------
(add-hook 'emacs-lisp-mode-hook
          #'(lambda ()
              (eldoc-mode 1)
              (flymake-mode 1)
              (setq elisp-flymake-byte-compile-load-path load-path)
              (diminish 'eldoc-mode)))

;; morlock --- more font-lock keywords for elisp -------------------------------
(use-package morlock
  :ensure t
  :config
  (morlock-mode 1))

(provide 'lambda-emacs-lisp)

;;; lambda-emacs-lisp.el ends here
