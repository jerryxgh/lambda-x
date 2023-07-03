;;; lambda-package.el --- packages about settings -*- lexical-binding: t -*-

;; Copyright (C) 2022 Guanghui Xu
;;
;; Author: Guanghui Xu gh_xu@qq.com
;; Maintainer: Guanghui Xu gh_xu@qq.com
;; Created: 2022-09-18
;; Version: 0.1
;; Keywords: package
;; Homepage: not distributed yet
;; Package-Version: 0.1
;; Package-Requires:
;;

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'lambda-package)

;;; Change Log:

;; Version $(3) 2022-09-18 GuanghuiXu
;;   - Initial release

;;; Code:

(require 'package)

(defconst lambda-x-direcotry (file-name-directory
                              (or load-file-name (buffer-file-name)))
  "Root directory of lambda-x.")

(defvar lambda-package-installed-packages nil
  "Pakcages installed through `lambda-package-ensure-install'.

This value is set automaticly, DONT set by hand.")


;; place package files relative to configuration directory
(setq package-user-dir (expand-file-name "packages/elpa" lambda-x-direcotry))

(add-to-list 'package-archives
            '("melpa" . "https://melpa.org/packages/"))
(setq package-archive-priorities
      '(("melpa" . 50)
        ("gnu" . 50)
        ("nongnu" . 0)))

;; do not auto load packages
(setq package-enable-at-startup nil)

;; Load packages explictly
(package-initialize)
(setq warning-suppress-log-types '((package reinitialization)))

(defun lambda-package-ensure-install (package)
  "This is like `package-install', but skip PACKAGE if it has been installed.

The difference is that if PACKAGE is already installed(checked through
 `package-installed-p'), it will not be installed again."
  (add-to-list 'lambda-package-installed-packages package)
  (unless (or (member package package-activated-list)
              (package-installed-p package))
    (message "Installing %s" (symbol-name package))
    (when (not package-archive-contents)
      (package-refresh-contents))
    (package-install package)))

(defun lambda-package-list-packages ()
  "Browse packages installed through function `lambda-package-ensure-install'."
  (interactive)
  (package-show-package-list lambda-package-installed-packages))

(defun lambda-package-list-auto-packages ()
  "Browse packages auto installed due to the dependence."
  (interactive)
  (package-show-package-list
   (-filter #'(lambda (p)
                (not (memq p lambda-package-installed-packages)))
            package-activated-list)))

(defun lambda-package-get-used-pkgs ()
  "Get all packages manually installed.
Including requirements and requirements of requirements.  Which means get all
used packages, this is mainly for getting unused packages."
  (delete-dups (-flatten
                (-map 'lambda-package-get-pkg-with-reqs
                      lambda-package-installed-packages))))

(defun lambda-package-get-pkg-with-reqs (package)
  "Get PACKAGE and requirements of PACKAGE and requirements of requirements."
  (if (and package
           (not (assq package package--builtins)))
      (cons package (-flatten
                     (-map #'(lambda (req)
                               (lambda-package-get-pkg-with-reqs (car req)))
                           (let ((pkg-desc
                                  (or (if (package-desc-p package) package)
                                      (cadr (assq package package-alist)))))
                             (if pkg-desc
                                 (package-desc-reqs pkg-desc))))))))

(defun lambda-package-list-unused-packages ()
  "Browse packages not used."
  (interactive)
  (package-show-package-list
   (let ((used-packages (lambda-package-get-used-pkgs)))
     (-filter #'(lambda (p)
                  (not (memq p used-packages)))
              package-activated-list))))

(lambda-package-ensure-install 'use-package)
(setq use-package-always-ensure t)

(use-package compat
  :ensure t)

(use-package dash
  :ensure t)

(provide 'lambda-package)

;;; lambda-package.el ends here
