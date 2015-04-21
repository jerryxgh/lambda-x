;;; lambda-cc.el --- c&c++
;; Time-stamp: <2015-04-15 16:19:02 Jerry Xu>
;;; Commentary:

;;; Code:

(require 'lambda-core)
(require 'cc-mode)

(setq c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
                        (c-mode . "k&r")
                        (c++-mode . "stroustrup")
                        (other . "linux")))

(define-key c-mode-base-map (kbd "RET") 'c-context-line-break)

(defvar lambda-cc-system-include
  (split-string
  "c:\mingw\bin\../lib/gcc/mingw32/4.8.1/include/c++
 c:\mingw\bin\../lib/gcc/mingw32/4.8.1/include/c++/mingw32
 c:\mingw\bin\../lib/gcc/mingw32/4.8.1/include/c++/backward
 c:\mingw\bin\../lib/gcc/mingw32/4.8.1/include
 c:\mingw\bin\../lib/gcc/mingw32/4.8.1/../../../../include
 c:\mingw\bin\../lib/gcc/mingw32/4.8.1/include-fixed
 c:\mingw\bin\../lib/gcc/mingw32/4.8.1/../../../../mingw32/include")
  "System include directories for c&c++.  This is got by \
echo \"\" | g++ -v -x c++ -E -")

(setq semantic-default-submodes '(global-semanticdb-minor-mode
                                  global-semantic-idle-scheduler-mode
                                  global-semantic-idle-summary-mode
                                  ;;global-semantic-decoration-mode
                                  ;;global-semantic-highlight-func-mode
                                  ;;global-semantic-stickyfunc-mode
                                  global-semantic-mru-bookmark-mode
                                  ))
(semantic-mode 1)
;(global-semantic-highlight-edits-mode 1)
;(global-semantic-idle-local-symbol-highlight-mode 1)
;;(global-semantic-idle-breadcrumbs-mode 1)
;;(global-semantic-show-unmatched-syntax-mode 1)
;(global-semantic-show-parser-state-mode 1)
(setq semanticdb-default-save-directory
      (expand-file-name "semanticdb" lambda-savefile-dir))
(setq ede-project-placeholder-cache-file
      (expand-file-name "ede-projects.el" lambda-savefile-dir))
;(semanticdb-enable-gnu-global-databases 'c-mode)
;(semanticdb-enable-gnu-global-databases 'c++-mode)
(add-hook 'semantic-init-hook
          '(lambda ()
             ;; C&C++
             (if (eq system-type 'windows-nt)
                 (mapc (lambda (dir)
                         (semantic-add-system-include dir 'c-mode)
                         (semantic-add-system-include dir 'c++-mode))
                       lambda-cc-system-include))))

;; auto-complete-clang --- auto complete backend for  c&c++ --------------------
(lambda-package-ensure-install 'auto-complete-clang)
(require 'auto-complete-clang)

;; google-c-style --------------------------------------------------------------
(lambda-package-ensure-install 'google-c-style)
(require 'google-c-style)

(add-hook 'c-mode-common-hook
          (lambda ()
            ;;(setq ac-sources (append
            ;;(list 'ac-source-gtags
            ;;'ac-source-semantic 'ac-source-semantic-raw) ac-sources))
            (setq ac-sources
                  (append '(;;ac-source-gtags
                            ;;ac-source-semantic
                            ;;ac-source-yasnippet
							)
                          ac-sources))
            ;;(setq flycheck-clang-include-path
                           ;;(list (expand-file-name "~/local/include/")))
			(add-to-list 'c-cleanup-list 'defun-close-semi)
			;; (c-toggle-auto-newline 1)
            (c-set-style "stroustrup")
			(c-toggle-hungry-state 1)
            ;;(google-set-c-style)
            ))

(add-hook 'c-mode-hook
          (lambda ()
            "Use clang to complete c."
            (add-to-list 'ac-sources 'ac-source-clang t)))

;; (setq ac-clang-flags
;; 	  (mapcar (lambda (item) (concat "-I" item))
;; 			  lambda-cc-system-include))

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
(lambda-package-ensure-install 'ggtags)
(if (featurep 'evil)
    (define-key evil-normal-state-map
      (kbd "M-.") 'ggtags-find-tag-dwim))
(add-hook 'c-mode-common-hook
	  (lambda ()
	    (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
	      (ggtags-mode 1)
	      ;; do not echo help message when point is on a tag, it's annoying
	      (if (get 'ggtags-active-tag 'help-echo)
		  (put 'ggtags-active-tag 'help-echo nil))
	      (diminish 'ggtags-mode))))

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
