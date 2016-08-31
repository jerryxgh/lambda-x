;;; lambda-init.el --- Emacs configuration start point.
;; Time-stamp: <2016-08-31 20:31:10 Guanghui Xu>

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

(defvar lambda-libraries
  '(
    ;; core settings, shared by all other modules
    lambda-core
    ;; loading modules
    lambda-evil
    lambda-cc
    lambda-blog
    lambda-web
    lambda-emacs-lisp
    lambda-scheme
    lambda-java
    lambda-python
    lambda-js
    lambda-json
    lambda-octave
    lambda-lua
    lambda-scala
    lambda-haskell
    lambda-vb
    lambda-eden
    lambda-individual

    ;; this should be loaded at last, restore buffers, minibuffer history, last
    ;; place of cursor, etc.
    lambda-session
    )
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


(provide 'lambda-init)

;;; lambda-init.el ends here
