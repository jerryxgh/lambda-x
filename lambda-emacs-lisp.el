;;; lambda-emacs-lisp.el --- emacs lisp
;; Time-stamp: <2024-09-16 22:38:19 Guanghui Xu>
;;; Commentary:

;;; Code:

(require 'lambda-core)

;; eldoc --- show emacs lisp doc in the minibuffer -----------------------------
(add-hook 'emacs-lisp-mode-hook
          #'(lambda ()
              (eldoc-mode 1)
              (flymake-mode 1)
              (setq elisp-flymake-byte-compile-load-path load-path)
              (diminish 'eldoc-mode)
              (setq-default flycheck-emacs-lisp-load-path load-path)))

;; morlock --- more font-lock keywords for elisp -------------------------------
(use-package morlock
  :ensure t
  :config
  (font-lock-add-keywords 'emacs-lisp-mode
                          morlock-el-font-lock-keywords))

(provide 'lambda-emacs-lisp)

;;; lambda-emacs-lisp.el ends here
