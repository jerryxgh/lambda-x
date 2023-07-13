;;; lambda-helm.el --- helm configs -*- lexical-binding: t -*-

;;; Commentary:
;; helm configs

;;; Code:


;; helm ------------------------------------------------------------------------
;; (lambda-package-ensure-install 'helm)
(use-package helm
  :ensure
  :custom
  ;; always split window for helm
  (helm-split-window-inside-p t)
  (helm-move-to-line-cycle-in-source t)
  :config
  ;; must set before helm-config,  otherwise helm use default
  ;; prefix "C-x c", which is inconvenient because you can
  ;; accidentially pressed "C-x C-c"
  (setq-default helm-command-prefix-key "C-c h"
                ;; use ido-at-point
                helm-mode-handle-completion-in-region nil)
  (helm-mode 1)

  (define-key shell-mode-map (kbd "C-c C-l") 'helm-comint-input-ring)
  (add-hook 'eshell-mode-hook
            #'(lambda ()
                (define-key eshell-mode-map (kbd "C-c C-l") 'helm-eshell-history)))

  ;; to use with ido, customize helm-completing-read-handlers-alist
  (setq-default helm-completing-read-handlers-alist
                '((describe-function . ido)
                  (describe-variable . ido)
                  (where-is . ido)
                  (load-library . ido)
                  (debug-on-entry . ido)
                  (dired-do-copy . ido)
                  (dired-do-rename . ido)
                  (dired-create-directory . ido)
                  (find-function . ido)
                  (find-tag . ido)
                  (find-file . ido)
                  (find-file-other-window . ido)
                  (switch-to-buffer . ido)
                  (httpd-serve-directory . ido)
                  (ffap-alternate-file . nil)
                  (tmm-menubar . nil)))
  )

;; M-x helm-projectile-ag
(use-package helm-projectile
  :ensure t
  :config
  (helm-projectile-on)
  :pin melpa)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGE: helm-descbinds                      ;;
;;                                              ;;
;; GROUP: Convenience -> Helm -> Helm Descbinds ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package helm-descbinds
  :ensure t
  :config
  (helm-descbinds-mode 1))

(use-package helm-ag
  :ensure t
  :custom
  (helm-ag-insert-at-point 'symbol))

(use-package ag
  :ensure t
  :config
  (setq ag-highlight-search t)
  (setq ag-reuse-window t))

(use-package helm-flycheck
  :ensure
  :config
  (define-key flycheck-mode-map (kbd "C-c ! h") 'helm-flycheck))

;; (use-package helm-flymake
;;   :ensure
;;   :config
;;   )

;; integrate with helm
(use-package helm-lsp
  :ensure
  :commands helm-lsp-workspace-symbol
  :config
  (define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol))

;; xref complete by helm
(use-package helm-xref
  :ensure)

(provide 'lambda-helm)

;;; lambda-helm.el ends here
