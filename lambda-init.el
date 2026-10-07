;;; lambda-init.el --- Emacs configuration start point. -*- lexical-binding: t -*-

;;; Commentary:

;; Values to adapt to different platforms:
;; system-type: OS type
;; emacs-version: just as it's name applies
;; executable-find: find whether there is a command in PATH.

;; This file handles Emacs initialization things, so should be simple enough,
;; other specific funtions should in corresponding modules.

;;; Code:

(when (version< emacs-version "30.1")
  (error "lambda-x requires Emacs 30.1 or newer (running %s)" emacs-version))

(add-to-list 'load-path
             (file-name-directory (or load-file-name (buffer-file-name))))

(defvar lambda-libraries
  '(
    lambda-package
    ;; core settings, shared by all other modules
    lambda-core
    lambda-tramp
    lambda-evil
    lambda-tty
    lambda-treemacs

    lambda-vertico
    lambda-corfu

    ;; lambda-ivy
    ;; lambda-company

    lambda-semantic
    lambda-treesit

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
    lambda-python

    lambda-bison

    lambda-eglot
    lambda-dired-subtree

    ;; lambda-dap
    ;; lambda-csharp
    ;; lambda-octave
    ;; lambda-haskell
    ;; lambda-evil-im

    ;; this should be loaded at last, restore buffers, minibuffer history, last
    ;; place of cursor, etc.
    lambda-session)
  "Libraries to be loaded of lambda-x.")

;; load libraries and show the progress
(let ((progress-reporter
       (make-progress-reporter "[lambda-x]: loading..."
                               0 (length lambda-libraries)))
      (progress 0))
  (dolist (library lambda-libraries)
    (require library)
    (progress-reporter-update progress-reporter (setq progress (1+ progress))))
  (progress-reporter-done progress-reporter))

(provide 'lambda-init)

;;; lambda-init.el ends here
