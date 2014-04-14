;;; lambda-web.el --- Web
;; Time-stamp: <2014-04-13 12:57:55 Jerry Xu>
;;; Commentary:

;;; Code:

(require 'lambda-core)

(lambda-package-ensure-install 'web-mode)
(require 'web-mode)

(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.blade\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ftl?\\'" . web-mode))
(add-to-list 'auto-mode-alist
'("/\\(views\\|html\\|theme\\|templates\\)/.*\\.php\\'" . web-mode))

(setq web-mode-markup-indent-offset 4)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 4)
(setq web-mode-disable-autocompletion t)

;; less-css-mode --------------------------------------------------------------
(lambda-package-ensure-install 'less-css-mode)
(require 'less-css-mode)
(setq less-css-compile-at-save t)

;; rainbow-mode ---------------------------------------------------------------
(lambda-package-ensure-install 'rainbow-mode)
(add-hook 'css-mode-hook 'rainbow-mode)


(provide 'lambda-web)
;;; lambda-web.el ends here
