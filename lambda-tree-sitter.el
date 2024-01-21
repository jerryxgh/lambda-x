;;; lambda-tree-sitter.el --- tree-sitter configuration -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'treesit)
(require 'lambda-package)

;; (use-package tree-sitter
;;   :ensure t
;;   :custom-face
;;   (tree-sitter-hl-face:operator      ((t (:inherit default))))
;;   ;; (tree-sitter-hl-face:constant        ((t (:foreground "LimeGreen"))))
;;   ;; c 语言结构体
;;   (tree-sitter-hl-face:property        ((t (:inherit font-lock-variable-name-face))))
;;   (tree-sitter-hl-face:function.call ((t (:inherit font-lock-function-name-face
;;                                                    :underline t
;;                                                    :italic t))))
;;   (tree-sitter-hl-face:function.method.call ((t)))
;;   (tree-sitter-hl-face:method.call   ((t (:inherit font-lock-function-name-face))))
;;   :config
;;   (global-tree-sitter-mode)
;;   (setq tsc-dyn-get-from '(:github))
;;   (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

;; (use-package tree-sitter-langs
;;   :ensure t
;;   :after tree-sitter)

(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")
        (java "https://github.com/tree-sitter/tree-sitter-java")
        (sql "https://github.com/DerekStride/tree-sitter-sql" "gh-pages")
        ))
;; (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))
;; (setq treesit-load-name-override-list '((js "libtree-sitter-js" "tree_sitter_javascript")))

(setq major-mode-remap-alist
      '((yaml-mode . yaml-ts-mode)
        (bash-mode . bash-ts-mode)
        (js2-mode . js-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (json-mode . json-ts-mode)
        (css-mode . css-ts-mode)
        (java-mode . java-ts-mode)
        (sql-mode . sql-ts-mode)))
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))

(provide 'lambda-tree-sitter)

;;; lambda-tree-sitter.el ends here
