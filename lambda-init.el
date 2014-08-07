;;; lambda-init.el --- start point

;;; Commentary:
;; system-type: OS type
;; emacs-version: just as it's name applies
;; executable-find: find whether there is a command in PATH
;; start point of all settings

;;; Code:

(defconst lambda-x-direcotry (file-name-directory
                              (or load-file-name (buffer-file-name)))
  "Root directory of lambda-x.")

(add-to-list 'load-path lambda-x-direcotry)
(add-to-list 'load-path (expand-file-name "non-elpa/" lambda-x-direcotry))

;; Load modules
(require 'lambda-core)
(require 'lambda-cc)
(require 'lambda-web)
(require 'lambda-writing)
(require 'lambda-emacs-lisp)
(require 'lambda-java)
(require 'lambda-python)
(require 'lambda-js)
(require 'lambda-json)
(require 'lambda-matlab)
(require 'lambda-ecb)
(require 'lambda-eden)
;; This should be loaded at last, restore buffers, minibuffer history, last
;; place of cursor
(require 'lambda-session)

(provide 'lambda-init)

;;; lambda-init.el ends here
