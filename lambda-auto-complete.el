;;; lambda-auto-complete.el --- auto-complete configuration -*- lexical-binding: t -*-

;; Copyright (C) 2018 Guanghui Xu
;;
;; Author: Guanghui Xu gh_xu@qq.com
;; Maintainer: Guanghui Xu gh_xu@qq.com
;; Created: 2018-08-17
;; Version: 0.0.1
;; Keywords: auto-complete
;; Homepage: not distributed yet
;; Package-Version: 0.0.1
;; Package-Requires: auto-complete
;;

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'lambda-auto-complete)

;;; Change Log:

;; Version $(3) 2018-08-17 GuanghuiXu
;;   - Initial release

;;; Code:

(require 'lambda-core)

;; auto-complete ---------------------------------------------------------------
(lambda-package-ensure-install 'auto-complete)
(require 'auto-complete)
;; (lambda-package-ensure-install 'ac-dabbrev)
(add-to-list 'ac-dictionary-directories
             (expand-file-name "ac-dict" lambda-x-direcotry))
(add-to-list 'ac-dictionary-files
             (expand-file-name "ac-dict/auto-complete.dict" lambda-x-direcotry))

;; (ac-config-default)

;; (setq-default ac-expand-on-auto-complete nil)
;; (setq-default ac-auto-start nil)
;; To get pop-ups with docs even if a word is uniquely completed
;; (setq-default ac-dwim nil)
(setq tab-always-indent 'complete)  ;; use 't when auto-complete is disabled
(add-to-list 'completion-styles 'initials t)
;; Stop completion-at-point from popping up completion buffers so eagerly
(require 'cc-vars)
(setq completion-cycle-threshold 5
      c-tab-always-indent nil
      c-insert-tab-function 'indent-for-tab-command)

;; Exclude very large buffers from dabbrev
(require 'dabbrev)
(setq dabbrev-friend-buffer-function
      #'(lambda (other-buffer) (< (buffer-size other-buffer) (* 1 1024 1024))))

(dolist (mode '(
                autoconf-mode
                change-log-mode
                clojure-mode
                cmake-mode
                conf-javaprop-mode
                conf-xdefaults-mode
                css-mode
                csv-mode
                eshell-mode
                espresso-mode
                git-commit-mode
                graphviz-dot-mode
                haml-mode
                haskell-mode
                html-mode
                inferior-emacs-lisp-mode
                js2-mode
                js3-mode
                json-mode
                less-css-mode
                lisp-mode
                log-edit-mode
                makefile-automake-mode
                makefile-bsdmake-mo
                makefile-gmake-mode
                markdown-mode
                nginx-mode
                nxml-mode
                objc-mode
                octave-mode
                org-mode
                rjsx-mode
                sass-mode
                sh-mode
                shell-mode
                smarty-mode
                snippet-mode
                sql-interactive-mode
                sql-mode
                text-mode
                textile-mode
                tuareg-mode
                vbnet-mode
                yaml-mode
                ))
  (add-to-list 'ac-modes mode))

(setq ac-comphist-file (expand-file-name "ac-comphist.dat"
                                         lambda-auto-save-dir)
      ;; ac-auto-start 3
      ac-ignore-case nil
      ac-use-menu-map t)

(define-key ac-mode-map (kbd "M-/") 'auto-complete)
(define-key ac-completing-map (kbd "<tab>") 'ac-expand)
(define-key ac-completing-map (kbd "<backtab>") 'ac-previous)
(define-key ac-completing-map (kbd "<return>") 'ac-complete)

;; pos-tip ---------------------------------------------------------------------
(lambda-package-ensure-install 'pos-tip)
(require 'pos-tip)
(setq ac-quick-help-prefer-pos-tip t)

;;popup ------------------------------------------------------------------------
(lambda-package-ensure-install 'popup)
(require 'popup)
;; add some shotcuts in popup menu mode
(define-key popup-menu-keymap (kbd "M-n") 'popup-next)
(define-key popup-menu-keymap (kbd "<tab>") 'popup-next)
(define-key popup-menu-keymap (kbd "<backtab>") 'popup-previous)
(define-key popup-menu-keymap (kbd "M-p") 'popup-previous)
(define-key popup-menu-keymap (kbd "<escape>")
  #'(lambda nil
      (interactive)
      (if (featurep 'evil)
          (evil-force-normal-state))
      (keyboard-quit)))

(defun yas-popup-isearch-prompt (prompt choices &optional display-fn)
  "Use popup to prompt PROMPT and CHOICES.
DISPLAY-FN: use this function to display."
  (when (featurep 'popup)
    (popup-menu*
     (mapcar
      #'(lambda (choice)
          (popup-make-item
           (or (and display-fn (funcall display-fn choice))
               choice)
           :value choice))
      choices)
     :prompt prompt
     :scroll-bar t
     ;; start isearch mode immediately
     ;;:isearch t
     )))

(add-to-list 'yas-prompt-functions 'yas-popup-isearch-prompt)

;; ac-capf https://github.com/emacsorphanage/ac-capf
(lambda-package-ensure-install 'ac-capf)
(require 'ac-capf)
(setq-default ac-sources (append '(ac-source-filename ac-source-dictionary ac-source-capf) ac-sources))

(provide 'lambda-auto-complete)

;;; lambda-auto-complete.el ends here
