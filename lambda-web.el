;;; lambda-web.el --- Web -*- lexical-binding: t -*-

;;; Commentary:
;; To use eglot, install git@code.byted.org:ecom/ecam-ai-assistant.git
;; npm install -g typescript-language-server typescript

;;; Code:

(require 'lambda-core)
(require 'lambda-eglot)

(use-package web-mode
  :ensure t
  :config
  (require 'web-mode))

(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[gj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.blade\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[x]html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ftl\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.xml\\'" . web-mode))
;; for velocity template engine script
(add-to-list 'auto-mode-alist '("\\.vm\\'" . web-mode))

;; Use web-mode instead of html-mode.
(setq auto-mode-alist
      (delete
       '("\\.[sx]?html?\\(\\.[a-zA-Z_]+\\)?\\'" . html-mode) auto-mode-alist))

(setq web-mode-markup-indent-offset 4)
(setq web-mode-css-indent-offset 4)
(setq web-mode-code-indent-offset 4)
;; (setq web-mode-disable-autocompletion t)

(add-hook 'web-mode-hook #'(lambda ()
                            "Disable auto-fill-mode and add HTML snippets."
                            (auto-fill-mode -1)
                            (make-local-variable 'yas-extra-modes)
                            (add-to-list 'yas-extra-modes 'html-mode)
                            ;; (setq ac-sources
                            ;;       (append '(ac-source-imenu
                            ;;                 ac-source-yasnippet
                            ;;                 ac-source-words-in-same-mode-buffers)
                            ;;               ac-sources))
                            ))

;; Work with auto-complete
;; (setq ac-modes (append ac-modes '(web-mode)))

;; less-css-mode --------------------------------------------------------------
(use-package less-css-mode
  :custom
  (less-css-compile-at-save t)
  :ensure t
  :config
  (require 'less-css-mode))

;; rainbow-mode ---------------------------------------------------------------
(use-package rainbow-mode
  :ensure t
  :hook ((prog-mode . (lambda ()
                        (rainbow-mode 1)
                        (diminish 'rainbow-mode)))))

(use-package typescript-mode
  :ensure t)

(defun lambda-typescript-select-mode ()
  "Use Tree-sitter for TypeScript when its grammar is ready."
  (if (treesit-ready-p 'typescript t)
      (typescript-ts-mode)
    (typescript-mode)))

(defun lambda-tsx-select-mode ()
  "Use Tree-sitter for TSX when its grammar is ready."
  (if (treesit-ready-p 'tsx t)
      (tsx-ts-mode)
    (typescript-mode)))

(use-package typescript-ts-mode
  :ensure nil

  :mode
  (("\\.ts\\'" . lambda-typescript-select-mode)
   ("\\.tsx\\'" . lambda-tsx-select-mode))

  :custom
  (typescript-ts-mode-indent-offset 2)

  :hook
  ((typescript-mode . eglot-ensure)
   (typescript-ts-mode . eglot-ensure)
   (tsx-ts-mode . eglot-ensure)))

(provide 'lambda-web)

;;; lambda-web.el ends here
