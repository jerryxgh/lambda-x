;;; lambda-emacs-lisp.el --- emacs lisp
;; Time-stamp: <2023-02-08 23:29:57 bytedance>
;;; Commentary:

;;; Code:

(require 'lambda-core)

;; eldoc --- show emacs lisp doc in the minibuffer -----------------------------
(add-hook 'emacs-lisp-mode-hook
          #'(lambda ()
              (eldoc-mode 1)
              (diminish 'eldoc-mode)
              (setq-default flycheck-emacs-lisp-load-path load-path)))

;; elisp-slime-nav --- Make M-. and M-, work in elisp like they do in slime-----
(lambda-package-ensure-install 'elisp-slime-nav)
;; (define-key elisp-slime-nav-mode-map (kbd "M-,") ())
;; (define-key elisp-slime-nav-mode-map (kbd "C-t") 'pop-tag-mark)
(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook #'(lambda ()
                     (turn-on-elisp-slime-nav-mode)
                     (diminish 'elisp-slime-nav-mode))))

;; morlock --- more font-lock keywords for elisp -------------------------------
(lambda-package-ensure-install 'morlock)
(require 'morlock)
(font-lock-add-keywords 'emacs-lisp-mode
                        morlock-el-font-lock-keywords)


(provide 'lambda-emacs-lisp)

;;; lambda-emacs-lisp.el ends here
