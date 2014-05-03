;;; init.el --- start point

;;; Commentary:
;; system-type: OS type
;; emacs-version: just as it's name applies
;; executable-find: find whether there is a command in PATH
;; start point of all settings

;;; Code:

(defconst lambda-x-direcotry (file-name-directory
                              (or load-file-name (buffer-file-name)))
  "Root directory of lambda-x.")


(require 'package)
;; Place package files in git repository directory
(setq package-user-dir (expand-file-name "elpa" lambda-x-direcotry))
;; Add more package sources
(dolist (pkg-arch '(;;("marmalade" . "http://marmalade-repo.org/packages/")
                    ;;("org" . "http://orgmode.org/elpa/")
                    ("melpa" . "http://melpa.milkbox.net/packages/")))
         (add-to-list 'package-archives pkg-arch))

;; Do not auto load packages
(setq package-enable-at-startup nil)
;; Load packages explictly
(package-initialize)

(defun lambda-package-ensure-install (package)
  "This is like `package-install', the difference is that if PACKAGE is \
already installed(checked through `package-installed-p'), it will not be \
installed again."
  (unless (or (member package package-activated-list)
              (package-installed-p package)
              (featurep package)
              (functionp package))
    (message "Installing %s" (symbol-name package))
    (when (not package-archive-contents)
      (package-refresh-contents))
    (package-install package)))

(add-to-list 'load-path lambda-x-direcotry)
(add-to-list 'load-path (concat lambda-x-direcotry "non-elpa/"))

;; Load modules
(require 'lambda-core)
(require 'lambda-cc)
(require 'lambda-web)
(require 'lambda-writing)
(require 'lambda-emacs-lisp)
(require 'lambda-java)
(require 'lambda-python)
;; This should be loaded at last, restore buffers, minibuffer history, last
;; place of cursor
(require 'lambda-session)


(provide 'init)

;;; init.el ends here
