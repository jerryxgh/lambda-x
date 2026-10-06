;;; lambda-java.el --- Java -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'lambda-core)
(require 'lambda-cc)
(require 'lambda-eglot)

;; Preserve the installed package's contact function while sharing it with TS.
(with-eval-after-load 'eglot
  (dolist (entry eglot-server-programs)
    (when (or (eq (car entry) 'java-mode)
              (and (listp (car entry)) (memq 'java-mode (car entry))))
      (setcar entry (delete-dups
                     (append (if (listp (car entry)) (car entry) '(java-mode))
                             '(java-ts-mode)))))))

(use-package eglot-java
  :ensure t
  :hook ((java-mode . eglot-java-mode)
         (java-ts-mode . eglot-java-mode))
  :custom
  (java-ts-mode-indent-offset 4))

(provide 'lambda-java)

;;; lambda-java.el ends here
