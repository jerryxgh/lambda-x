;;; lambda-bison.el --- flex and bison major mode -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package bison-mode
  :ensure
  ;; :hook ((bisin-mode . (lambda ()
  ;;                        (setq company-backends
  ;;                              '((lambda-company-yasnippet lambda-company-dabbrev-code lambda-company-dabbrev lambda-company-keywords)))
  ;;                        ))
  ;;        (flex-mode . (lambda ()
  ;;                       (setq company-backends
  ;;                             '((lambda-company-yasnippet lambda-company-dabbrev-code lambda-company-dabbrev lambda-company-keywords)))
  ;;                       )))
  :mode (("\\.y\\'" . bison-mode)
         ("\\.yy\\'" . bison-mode)
         ("\\.l\\'" . flex-mode)
         ("\\.ll\\'" . flex-mode)
         )
  :pin melpa
  )

;; (add-hook 'semantic-grammar-mode-hook
;;           (lambda ()
;;             (setq company-backends
;;                   '((lambda-company-yasnippet lambda-company-dabbrev-code lambda-company-dabbrev lambda-company-keywords)))
;;             ))

(provide 'lambda-bison)

;;; lambda-bison.el ends here
