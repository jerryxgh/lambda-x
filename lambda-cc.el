;;; lambda-cc.el --- c&c++
;; Time-stamp: <2015-07-07 16:48:01 Jerry Xu>
;;; Commentary:

;;; Code:

(require 'lambda-core)
(require 'cc-mode)

(setq c-default-style '((java-mode . "cc-mode")
                        (awk-mode . "awk")
                        (c-mode . "k&r")
                        (c++-mode . "stroustrup")
                        (other . "linux")))

(define-key c-mode-base-map (kbd "RET") 'c-context-line-break)

(setq semantic-default-submodes '(global-semanticdb-minor-mode
                                  global-semantic-idle-scheduler-mode
                                  global-semantic-idle-summary-mode
                                  ;;global-semantic-decoration-mode
                                  ;;global-semantic-highlight-func-mode
                                  ;;global-semantic-stickyfunc-mode
                                  global-semantic-mru-bookmark-mode
                                  ))
(semantic-mode 1)
;;(global-semantic-highlight-edits-mode 1)
;;(global-semantic-idle-local-symbol-highlight-mode 1)
;;(global-semantic-idle-breadcrumbs-mode 1)
;;(global-semantic-show-unmatched-syntax-mode 1)
;;(global-semantic-show-parser-state-mode 1)
(setq semanticdb-default-save-directory
      (expand-file-name "semanticdb" lambda-savefile-dir))
(setq ede-project-placeholder-cache-file
      (expand-file-name "ede-projects.el" lambda-savefile-dir))
;(semanticdb-enable-gnu-global-databases 'c-mode)
;(semanticdb-enable-gnu-global-databases 'c++-mode)

;; irony-mode ------------------------------------------------------------------
(lambda-package-ensure-install 'irony)
(setq irony-server-install-prefix (expand-file-name "irony" lambda-savefile-dir)
      irony-user-dir (expand-file-name "irony" lambda-savefile-dir))
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

(require 'ac-irony)
(add-hook 'irony-mode-hook
          #'(lambda ()
              (irony-cdb-autosetup-compile-options)
              (unless (memq 'ac-source-irony ac-sources)
                (setq ac-sources (append '(ac-source-irony) ac-sources)))))

(lambda-package-ensure-install 'flycheck-irony)
(lambda-package-ensure-install 'irony-eldoc)

;; google-c-style --------------------------------------------------------------
(lambda-package-ensure-install 'google-c-style)
(require 'google-c-style)

(add-hook 'c-mode-common-hook
          #'(lambda ()
              ;;(setq ac-sources (append
              ;;(list 'ac-source-gtags
              ;;'ac-source-semantic 'ac-source-semantic-raw) ac-sources))
              (add-to-list 'c-cleanup-list 'defun-close-semi)
              ;; (c-set-style "stroustrup")
              ;;(google-set-c-style)
              ;; (c-toggle-auto-newline 1)
              (c-toggle-hungry-state 1)))

;; ffap - find file at point ---------------------------------------------------
(autoload 'ffap-href-enable "ffap-href" nil t)
(eval-after-load "ffap" '(ffap-href-enable))

(autoload 'ffap-I-option-enable "ffap-I-option" nil t)
(eval-after-load "ffap" '(ffap-I-option-enable))
(eval-after-load "ffap" '(require 'ffap-include-start))
(eval-after-load "ffap" '(require 'ffap-gcc-path))

;; gdb configs -----------------------------------------------------------------
;; (add-hook 'gdb-mode-hook 'kill-buffer-when-shell-command-exit)
(require 'gud)
(define-key gud-mode-map (kbd "<f5>") 'gud-step)
(define-key gud-mode-map (kbd "<f6>") 'gud-next)
(define-key gud-mode-map (kbd "<f7>") 'gud-up)
(define-key gud-mode-map (kbd "<f8>") 'gud-go)
(setq gdb-show-main t
      gdb-many-windows t)

;; global ------- code navigating ----------------------------------------------
;; (lambda-package-ensure-install 'ggtags)
;; (if (featurep 'evil)
;;     (define-key evil-normal-state-map
;;       (kbd "M-.") 'ggtags-find-tag-dwim))
;; (add-hook 'c-mode-common-hook
;; 	  (lambda ()
;; 	    (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
;; 	      (ggtags-mode 1)
;; 	      ;; do not echo help message when point is on a tag, it's annoying
;; 	      (if (get 'ggtags-active-tag 'help-echo)
;; 		  (put 'ggtags-active-tag 'help-echo nil))
;; 	      (diminish 'ggtags-mode))))

(lambda-package-ensure-install 'helm-gtags)
;; Key 		Command
;; Prefix h 	helm-gtags-display-browser
;; Prefix C-] 	helm-gtags-find-tag-from-here
;; Prefix C-t 	helm-gtags-pop-stack
;; Prefix P 	helm-gtags-find-files
;; Prefix f 	helm-gtags-parse-file
;; Prefix g 	helm-gtags-find-pattern
;; Prefix s 	helm-gtags-find-symbol
;; Prefix r 	helm-gtags-find-rtag
;; Prefix t 	helm-gtags-find-tag
;; Prefix d 	helm-gtags-find-tag
;; M-* 		helm-gtags-pop-stack
;; M-. 		helm-gtags-find-tag
;; C-x 4 . 	helm-gtags-find-tag-other-window
(when (featurep 'evil)
  (define-key evil-normal-state-map
    (kbd "M-.") 'helm-gtags-dwim)
  (define-key evil-normal-state-map
    (kbd "C-t") 'helm-gtags-pop-stack))
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
              (helm-gtags-mode 1)
              (diminish 'helm-gtags-mode))))
;; customize
(custom-set-variables
 '(helm-gtags-path-style 'relative)
 '(helm-gtags-ignore-case t)
 '(helm-gtags-auto-update t))

;; cmake -----------------------------------------------------------------------
(lambda-package-ensure-install 'cpputils-cmake)
;;(require 'cpputils-cmake)
;; (add-hook 'c-mode-common-hook
;;           (lambda ()
;;             (if (derived-mode-p 'c-mode 'c++-mode)
;;                 ;; (cppcm-reload-all)
;;               )))

(provide 'lambda-cc)

;;; lambda-cc.el ends here
