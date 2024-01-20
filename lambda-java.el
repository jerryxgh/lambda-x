;;; lambda-java.el --- Java

;;; Commentary:

;;; Code:

(require 'lambda-core)
(require 'lambda-cc)
(require 'lambda-company)
(require 'semantic)
(require 'lambda-eglot)

;; to recognize mvn compile errors
(setq compilation-error-regexp-alist
      (delete
       '("^\\(.*\\):\\[\\([0-9]*\\),\\([0-9]*\\)\\]" 1 2 3)
       compilation-error-regexp-alist))

(add-hook 'java-mode-hook
          (lambda ()
            (semantic-mode 1)
            (setq company-backends
                  '((company-semantic
                     :with lambda-company-yasnippet lambda-company-dabbrev-code lambda-company-dabbrev lambda-company-keywords)))))

(provide 'lambda-java)

;;; lambda-java.el ends here
