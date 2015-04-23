;;; simplesite.el --- Simple static site generator -*- lexical-binding: t -*-

;; Copyright 2015 Jerry Xu
;;
;; Author: Jerry Xu gh_xu@qq.com
;; Maintainer: Jerry Xu gh_xu@qq.com
;; Version: 0.0
;; Keywords: blog, static site, generator
;; Homepage: not distributed yet

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Simple static site generator.

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'simplesite)

;;; Code:

(require 'ss-backends)
(require 'ss-index)
(require 'ss-post)
(require 'ss-tags)
(require 'ss-archive)
(require 'ss-categories)
(require 'ss-org-backend)

(defconst simplesite-version "0.1")

(defun ss-generate ()
  "Generate site."
  (interactive)
  ;; check important variables
  (ss--check-variables)
  ;; copy theme resource files
  ;;(ss-prepare-theme ss-dist-directory)
  (let ((file-tlist (ss-parse-all-src-files ss-source-directory
                                            ss-dist-directory)))
    ;; generate post if the file is changed, then release post-content
    (mapc #'(lambda (attr-table)
              (when (ht-get attr-table "post-content")
                (ss-generate-post attr-table)
                (ht-remove attr-table "post-content")
                (ht-remove attr-table "content")))
          file-tlist)

    (ss-generate-index file-tlist)
    (ss-generate-categories file-tlist)
    (ss-generate-tags file-tlist)
    (mapc #'(lambda (attr-table)
              (message (ht-get attr-table "date")))
          file-tlist)))

(defun ss--check-variables ()
  "Do some check before generate site."
  (unless (and ss-source-directory
               (file-directory-p ss-source-directory))
    (error "Directory `%s' is not properly configured" ss-source-directory))
  (unless (and ss-dist-directory
               (file-directory-p ss-dist-directory))
    (setq ss-dist-directory
          (f-parent ss-source-directory)))
  (setq ss-source-directory (directory-file-name ss-source-directory))
  (setq ss-dist-directory (directory-file-name ss-dist-directory)))

(defun ss--correct-links (post-content)
  "Correct links in exported POST-CONTENT.

TODO: not implemented."
  post-content)

;; test only
(setq ss-source-directory
      "/home/xgh/repository/lambda-x/non-elpa/simplesite/test/source"
      ss-author "Jerry")

(provide 'simplesite)

;;; simplesite.el ends here
