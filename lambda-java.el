;;; lambda-java.el --- Java

;;; Commentary:

;;; Code:

(require 'lambda-core)
(require 'lambda-cc)

;; to recognize mvn compile errors
(setq compilation-error-regexp-alist
      (delete
       '("^\\(.*\\):\\[\\([0-9]*\\),\\([0-9]*\\)\\]" 1 2 3)
       compilation-error-regexp-alist))

;; java-snippets A set of java-mode snippets for YASnippet. --------------------
(lambda-package-ensure-install 'java-snippets)

(provide 'lambda-java)

;;; lambda-java.el ends here
