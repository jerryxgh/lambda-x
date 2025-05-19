;;; lambda-treesit.el --- treesit configuration -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'treesit)
(require 'lambda-package)

;; from https://arnesonium.com/2023/08/configuring-emacs-29-1-for-golang
;; (setq treesit-language-source-alist
;;  '((bash "https://github.com/tree-sitter/tree-sitter-bash")
;;    (cmake "https://github.com/uyha/tree-sitter-cmake")
;;    (css "https://github.com/tree-sitter/tree-sitter-css")
;;    (elisp "https://github.com/Wilfred/tree-sitter-elisp")
;;    (go "https://github.com/tree-sitter/tree-sitter-go")
;;    (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
;;    (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
;;    (html "https://github.com/tree-sitter/tree-sitter-html")
;;    (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
;;    (json "https://github.com/tree-sitter/tree-sitter-json")
;;    (make "https://github.com/alemuller/tree-sitter-make")
;;    (markdown "https://github.com/ikatyang/tree-sitter-markdown")
;;    (python "https://github.com/tree-sitter/tree-sitter-python")
;;    (toml "https://github.com/tree-sitter/tree-sitter-toml")
;;    (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
;;    (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
;;    (yaml "https://github.com/ikatyang/tree-sitter-yaml")
;;    (java "https://github.com/tree-sitter/tree-sitter-java")
;;    (sql "https://github.com/DerekStride/tree-sitter-sql" "gh-pages")
;;    ))
;; (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))
;; (setq treesit-load-name-override-list '((js "libtree-sitter-js" "tree_sitter_javascript")))

(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  ;; (global-treesit-auto-mode)
  (add-hook 'go-ts-mode-hook (lambda ()
                               (setq tab-width 4))))

(provide 'lambda-treesit)

;;; lambda-treesit.el ends here
