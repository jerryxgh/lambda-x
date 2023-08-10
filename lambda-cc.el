;;; lambda-cc.el --- c&c++
;; Time-stamp: <2023-08-10 14:56:56 Guanghui Xu>
;;; Commentary:

;;; Code:

(require 'lambda-core)
(require 'lambda-evil)
(with-eval-after-load 'cc-mode
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

;; ffap - find file at point ---------------------------------------------------
(autoload 'ffap-href-enable "ffap-href" nil t)
(autoload 'ffap-I-option-enable "ffap-I-option" nil t)
(with-eval-after-load 'ffap
  (require 'ffap-include-start)
  (require 'ffap-gcc-path)
  (ffap-href-enable)
  (ffap-I-option-enable))

;; gdb configs -----------------------------------------------------------------
(with-eval-after-load 'gud
  (define-key gud-mode-map (kbd "<f5>") 'gud-step)
  (define-key gud-mode-map (kbd "<f6>") 'gud-next)
  (define-key gud-mode-map (kbd "<f7>") 'gud-up)
  (define-key gud-mode-map (kbd "<f8>") 'gud-go)
  )

(with-eval-after-load 'gdb-mi
  (setq gdb-many-windows t)
  )

;; cmake -----------------------------------------------------------------------
(use-package cmake-mode
  :ensure t)

(provide 'lambda-cc)

;;; lambda-cc.el ends here
