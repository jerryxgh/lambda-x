;;; lambda-evil-im.el --- Input method improving for evil. -*- lexical-binding: t -*-

;; Copyright (C) 2017 Guanghui Xu
;;
;; Author: Guanghui Xu gh_xu@qq.com
;; Maintainer: Guanghui Xu gh_xu@qq.com
;; Created: 2017-02-16
;; Version: 0.0.1alpha
;; Keywords: convenience
;; Homepage: not distributed yet
;; Package-Version: 0.0.1alpha
;; Package-Requires: ((emacs "24.4"))
;;

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'lambda-evil-im)

;;; Change Log:

;; Version $(3) 2017-02-16 GuanghuiXu
;;   - Initial release

;;; Code:

;; switch to english input method when switching to normal mode
;; and switch back when entering insert/replace modes
;; need external script support, currently mac-only
;; (defvar default-im " com.apple.keylayout.Dvorak " " Default ascii-only input method " )

(defvar lambda-evil-im-select-command "~/local/bin/im-select"
  "The im-select command, from https://github.com/ybian/smartim.")

(defvar lambda-evil-im-default-im "com.apple.keylayout.ABC"
  "Default ascii-only input method.")

(defvar lambda-evil-im-prev-im (substring (shell-command-to-string lambda-evil-im-select-command) 0 -1)
  "IM using when starting Emacs and exiting insert mode.")

(defun lambda-evil-im-use-default ()
  "Switch to default input method."
  (interactive )
  (cond ((eq system-type 'darwin )
         (call-process-shell-command (concat lambda-evil-im-select-command " "
                                             lambda-evil-im-default-im)))))

(defun lambda-evil-im-remember ()
  "Remember input method used in insert mode to switch back in other modes."
  (interactive )
  (cond ((eq system-type 'darwin)
         (setq lambda-evil-im-prev-im
               (substring (shell-command-to-string lambda-evil-im-select-command) 0 -1 )))))

(defun lambda-evil-im-use-prev ()
  "Use previous input method."
  (interactive )
  (cond ((eq system-type 'darwin)
         (if lambda-evil-im-prev-im
             (call-process-shell-command (concat lambda-evil-im-prev-im " "
                                                 lambda-evil-im-prev-im))
           (call-process-shell-command (concat lambda-evil-im-prev-im
                                               lambda-evil-im-default-im))))))

(add-hook 'evil-normal-state-entry-hook 'lambda-evil-im-use-default )
(add-hook 'evil-insert-state-entry-hook 'lambda-evil-im-use-prev )
(add-hook 'evil-insert-state-exit-hook 'lambda-evil-im-remember)
(add-hook 'evil-replace-state-entry-hook 'lambda-evil-im-use-prev )
(add-hook 'evil-replace-state-exit-hook 'lambda-evil-im-remember )
(add-hook 'evil-emacs-state-entry-hook 'lambda-evil-im-use-default )

(provide 'lambda-evil-im)

;;; lambda-evil-im.el ends here
