;;; lambda-js.el --- JavaScript

;;; Commentary:

;;; Code:

(require 'lambda-core)

;; rjsx-mode--------------------------------------------------------------------
(lambda-package-ensure-install 'rjsx-mode)
(add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode))

;; add-node-modules-path--------------------------------------------------------
;; suport eslint use project local eslint
(lambda-package-ensure-install 'add-node-modules-path)
(with-eval-after-load 'rjsx-mode
  (add-hook 'rjsx-mode-hook (lambda ()
                              (add-node-modules-path)
                              (when (executable-find "tern")
                                (tern-mode t)))))

;; tern: a stand-alone code-analysis engine for JavaScript ---------------------
;; Here we use tern and auto-complete to complete JavaScript
(lambda-package-ensure-install 'tern)
(when-let* ((tern-executable (executable-find "tern")))
  (setq tern-command (list tern-executable)))

;; (lambda-package-ensure-install 'tern-auto-complete)
;; (eval-after-load 'tern
;;   '(progn
;; 	 (require 'tern-auto-complete)
;; 	 (tern-ac-setup)
;; 	 (diminish 'tern-mode)))

;; nodejs-repl -----------------------------------------------------------------
(lambda-package-ensure-install 'nodejs-repl)
(require 'nodejs-repl)

;; yarn-mode
(lambda-package-ensure-install 'yarn-mode)
(require 'yarn-mode)

(provide 'lambda-js)

;;; lambda-js.el ends here
