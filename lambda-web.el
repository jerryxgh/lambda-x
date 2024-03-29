;;; lambda-web.el --- Web
;; Time-stamp: <2023-02-09 12:46:30 Guanghui Xu>
;;; Commentary:

;;; Code:

(require 'lambda-core)

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
(add-to-list 'auto-mode-alist '("\\.ftl?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.xml?\\'" . web-mode))
;; for velocity template engine script
(add-to-list 'auto-mode-alist '("\\.vm?\\'" . web-mode))

;; Use web-mode instead of html-mode.
(setq auto-mode-alist
      (delete
       '("\\.[sx]?html?\\(\\.[a-zA-Z_]+\\)?\\'" . html-mode) auto-mode-alist))

(setq web-mode-markup-indent-offset 4)
(setq web-mode-css-indent-offset 4)
(setq web-mode-code-indent-offset 4)
;; (setq web-mode-disable-autocompletion t)

(add-hook 'web-mode-hook #'(lambda ()
                            "Disable auto-fill-mode and fci-mode in web mode,
and set yas--extra-mode to use html snippets."
                            (auto-fill-mode -1)
                            (fci-mode -1)
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

(provide 'lambda-web)

;;; lambda-web.el ends here
