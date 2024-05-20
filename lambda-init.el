;;; lambda-init.el --- Emacs configuration start point.
;; Time-stamp: <2024-05-20 14:52:45 Guanghui Xu>

;;; Commentary:

;; Values to adapt to different platforms:
;; system-type: OS type
;; emacs-version: just as it's name applies
;; executable-find: find whether there is a command in PATH.

;; This file handles Emacs initialization things, so should be simple enough,
;; other specific funtions should in corresponding modules.

;;; Code:

(add-to-list 'load-path
             (file-name-directory (or load-file-name (buffer-file-name))))

;; maximize frame
(unless (frame-parameter nil 'fullscreen)
      (toggle-frame-maximized))
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))
(redisplay)

(defvar lambda-libraries
  '(
    lambda-package
    ;; core settings, shared by all other modules
    lambda-core
    lambda-ivy
    lambda-evil
    lambda-tty
    lambda-treemacs
    ;; lambda-corfu
    lambda-company
    lambda-semantic

    lambda-cc
    lambda-org
    lambda-web
    lambda-emacs-lisp
    lambda-scheme
    lambda-java
    lambda-groovy
    lambda-js
    lambda-json
    lambda-sql
    lambda-lua
    lambda-vb
    lambda-eden
    lambda-individual
    lambda-golang
    lambda-thrift
    ;; lambda-dap
    lambda-python

    lambda-bison

    lambda-eglot

    ;; lambda-csharp
    ;; lambda-octave
    ;; lambda-haskell
    ;; lambda-evil-im

    ;; this should be loaded at last, restore buffers, minibuffer history, last
    ;; place of cursor, etc.
    lambda-session)
  "Libraries to be loaded of lambda-x.")

;; load libraries and show the progress
(let* ((progress-reporter
        (make-progress-reporter "[lambda-x]: loading..." 0 100))
       (library-num (length lambda-libraries))
       (step (/ 99 library-num))
       (progress 0))

  (dolist (library lambda-libraries)
    (require library)
    (setq progress (+ progress step))
    (progress-reporter-update progress-reporter progress)
    (redisplay))

  (progress-reporter-done progress-reporter))

;;; tramp
(setq tramp-default-method "sshx")
(tramp-set-completion-function "sshx"
                               '((tramp-parse-sconfig "/etc/ssh_config")
                                 (tramp-parse-sconfig "~/.ssh/config")))
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)

(provide 'lambda-init)

;;; lambda-init.el ends here
