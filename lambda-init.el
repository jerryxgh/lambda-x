;;; lambda-init.el --- Emacs configuration start point.
;; Time-stamp: <2016-06-12 16:46:28 Guanghui Xu>

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

;; core settings, shared by all other modules
(require 'lambda-core)
;; loading modules
(require 'lambda-evil)
(require 'lambda-cc)
(require 'lambda-blog)
(require 'lambda-web)
(require 'lambda-emacs-lisp)
(require 'lambda-scheme)
(require 'lambda-java)
(require 'lambda-python)
(require 'lambda-js)
(require 'lambda-json)
(require 'lambda-octave)
(require 'lambda-lua)
(require 'lambda-scala)
(require 'lambda-haskell)
(require 'lambda-vb)
(require 'lambda-eden)

(require 'lambda-individual "lambda-individual.el" t)

;; this should be loaded at last, restore buffers, minibuffer history, last
;; place of cursor, etc.
(require 'lambda-session)

(provide 'lambda-init)

;;; lambda-init.el ends here
