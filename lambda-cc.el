;;; lambda-cc.el --- c&c++
;; Time-stamp: <2014-08-07 21:53:03 Jerry Xu>
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
                                  ;;global-semantic-idle-summary-mode
                                  ;;global-semantic-decoration-mode
                                  ;;global-semantic-highlight-func-mode
                                  ;;global-semantic-stickyfunc-mode
                                  ;;global-semantic-mru-bookmark-mode
                                  ))
(semantic-mode 1)
;(global-semantic-highlight-edits-mode 1)
;(global-semantic-idle-local-symbol-highlight-mode 1)
;;(global-semantic-idle-breadcrumbs-mode 1)
;;(global-semantic-show-unmatched-syntax-mode 1)
;(global-semantic-show-parser-state-mode 1)
(setq semanticdb-default-save-directory
      (expand-file-name "auto-save-list/semanticdb" user-emacs-directory))
(setq ede-project-placeholder-cache-file
      (expand-file-name "auto-save-list/ede-projects.el" user-emacs-directory))
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
			(c-toggle-hungry-state 1)))

(add-hook 'c-mode-hook
		  (lambda ()
			"Use clang to complete c."
			(setq ac-sources
                  (append '(ac-source-clang)
                          ac-sources))))

;; (setq ac-clang-flags
;; 	  (mapcar (lambda (item) (concat "-I" item))
;; 			  lambda-cc-system-include))


(provide 'lambda-cc)

;;; lambda-cc.el ends here
