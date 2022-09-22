;;; lambda-cc.el --- c&c++
;; Time-stamp: <2022-09-23 00:03:15 Guanghui Xu>
;;; Commentary:

;;; Code:

(require 'lambda-core)
(require 'lambda-evil)
(with-eval-after-load "cc-mode"
  (define-key c-mode-base-map (kbd "RET") 'c-context-line-break)
  (add-hook 'c-mode-common-hook
            #'(lambda ()
                ;;(setq ac-sources (append
                ;;(list 'ac-source-gtags
                ;;'ac-source-semantic 'ac-source-semantic-raw) ac-sources))
                (add-to-list 'c-cleanup-list 'defun-close-semi)
                (c-set-style "k&r")
                ;; https://stackoverflow.com/questions/13825188/suppress-c-namespace-indentation-in-emacs
                (defconst my-cc-style
                  '("k&r"
                    (c-offsets-alist . ((innamespace . [0])))))
                (c-add-style "my-cc-style" my-cc-style)
                (setq tab-width 8)
                (setq indent-tabs-mode nil)
                (setq c-basic-offset 4)
                ;; equal to c-toggle-auto-newline + c-toggle-hungry-state
                (c-toggle-electric-state 1)))

  (setq semantic-default-submodes '(global-semanticdb-minor-mode
                                    global-semantic-idle-scheduler-mode
                                    ;;global-semantic-idle-summary-mode
                                    ;;global-semantic-decoration-mode
                                    ;;global-semantic-highlight-func-mode
                                    ;;global-semantic-stickyfunc-mode
                                    global-semantic-mru-bookmark-mode
                                    ))
  ;;(semantic-mode 1)
  ;;(global-semantic-highlight-edits-mode 1)
  ;;(global-semantic-idle-local-symbol-highlight-mode 1)
  ;;(global-semantic-idle-breadcrumbs-mode 1)
  ;;(global-semantic-show-unmatched-syntax-mode 1)
  ;;(global-semantic-show-parser-state-mode 1)
  (require 'semantic/db-file)
  (setq semanticdb-default-save-directory
        (expand-file-name "semanticdb" lambda-auto-save-dir))
  ;; (setq ede-project-placeholder-cache-file
  ;;       (expand-file-name "ede-projects.el" lambda-auto-save-dir))
                                        ;(semanticdb-enable-gnu-global-databases 'c-mode)
                                        ;(semanticdb-enable-gnu-global-databases 'c++-mode)
  )

;; google-c-style --------------------------------------------------------------
(lambda-package-ensure-install 'google-c-style)
;; (require 'google-c-style)
;; (add-hook 'c-mode-common-hook
;;           #'(lambda ()
;;               (google-set-c-style)))

;; ffap - find file at point ---------------------------------------------------
(autoload 'ffap-href-enable "ffap-href" nil t)
(eval-after-load "ffap" '(ffap-href-enable))

(autoload 'ffap-I-option-enable "ffap-I-option" nil t)
(eval-after-load "ffap" '(ffap-I-option-enable))
(eval-after-load "ffap" '(require 'ffap-include-start))
(eval-after-load "ffap" '(require 'ffap-gcc-path))

;; gdb configs -----------------------------------------------------------------
(with-eval-after-load "gud"
  (define-key gud-mode-map (kbd "<f5>") 'gud-step)
  (define-key gud-mode-map (kbd "<f6>") 'gud-next)
  (define-key gud-mode-map (kbd "<f7>") 'gud-up)
  (define-key gud-mode-map (kbd "<f8>") 'gud-go)
  )

(with-eval-after-load "gdb-mi"
  (setq gdb-many-windows t)
  )


;; global ------- code navigating ----------------------------------------------
;; (lambda-package-ensure-install 'ggtags)
;; (if (featurep 'evil)
;;     (define-key evil-normal-state-map
;;       (kbd "M-.") 'ggtags-find-tag-dwim))
;; (add-hook 'c-mode-common-hook
;;        (lambda ()
;;          (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
;;            (ggtags-mode 1)
;;            ;; do not echo help message when point is on a tag, it's annoying
;;            (if (get 'ggtags-active-tag 'help-echo)
;;                (put 'ggtags-active-tag 'help-echo nil))
;;            (diminish 'ggtags-mode))))

(lambda-package-ensure-install 'helm-gtags)
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
              (helm-gtags-mode 1)
              (diminish 'helm-gtags-mode))))
(with-eval-after-load "helm-gtags"
  ;; Key          Command
  ;; Prefix h     helm-gtags-display-browser
  ;; Prefix C-]   helm-gtags-find-tag-from-here
  ;; Prefix C-t   helm-gtags-pop-stack
  ;; Prefix P     helm-gtags-find-files
  ;; Prefix f     helm-gtags-parse-file
  ;; Prefix g     helm-gtags-find-pattern
  ;; Prefix s     helm-gtags-find-symbol
  ;; Prefix r     helm-gtags-find-rtag
  ;; Prefix t     helm-gtags-find-tag
  ;; Prefix d     helm-gtags-find-tag
  ;; M-*          helm-gtags-pop-stack
  ;; M-.          helm-gtags-find-tag
  ;; C-x 4 .      helm-gtags-find-tag-other-window
  (setq helm-gtags-suggested-key-mapping t)

  ;; customize
  (custom-set-variables
   '(helm-gtags-path-style 'relative)
   '(helm-gtags-ignore-case t)
   '(helm-gtags-auto-update t))

  ;; key bindings
  ;; (define-key c-mode-base-map (kbd "M-.") 'helm-gtags-dwim)
  ;; (define-key c-mode-base-map (kbd "C-t") 'helm-gtags-pop-stack)
  (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
  (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
)


;; cmake -----------------------------------------------------------------------
(lambda-package-ensure-install 'cmake-mode)
(require 'cmake-mode)

(lambda-package-ensure-install 'cmake-font-lock)
(autoload 'cmake-font-lock-activate "cmake-font-lock" nil t)
(add-hook 'cmake-mode-hook 'cmake-font-lock-activate)

(provide 'lambda-cc)

;;; lambda-cc.el ends here
