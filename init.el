;;; init.el --- 

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
  "This is like `package-install', the difference is that if PACKAGE is already installed(checked through `package-installed-p'), it will not be installed again."
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

;;; init.el ends here
