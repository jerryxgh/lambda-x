;;; lambda-init.el --- Emacs configuration start point.
;; Time-stamp: <2015-05-12 22:19:33 Jerry Xu>

;;; Commentary:

;; Values to adapt to different platforms:
;; system-type: OS type
;; emacs-version: just as it's name applies
;; executable-find: find whether there is a command in PATH.

;; This file handles Emacs initialization things, so should be simple enough,
;; other specific funtions should in corresponding modules.

;;; Code:

(defconst current-user
  (getenv
   (if (equal system-type 'windows-nt) "USERNAME" "USER")))

(defconst lambda-x-direcotry (file-name-directory
                              (or load-file-name (buffer-file-name)))
  "Root directory of lambda-x.")

(defconst lambda-savefile-dir (expand-file-name "auto-save-list/"
                                                user-emacs-directory)
  "This folder stores all the automatically generated save/history-files.")

(add-to-list 'load-path lambda-x-direcotry)
(add-to-list 'load-path (expand-file-name "non-elpa" lambda-x-direcotry))
(add-to-list 'load-path "/home/xgh/repository/simplesite/")

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
(require 'lambda-eden)
;;; this should be loaded at last, restore buffers, minibuffer history, last
;;; place of cursor, etc.
(require 'lambda-session)

;;; init.el ends here
