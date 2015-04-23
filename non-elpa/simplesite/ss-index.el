;;; ss-index.el --- generate index page -*- lexical-binding: t -*-

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

;;; Code:

(require 'ss-theme)
(require 'ss-options)

(defun ss-generate-index (file-tlist)
  "Generate index page based on FILE-TLIST."
  (let ((mustache-partial-paths
         (list (ss-get-theme-template-dir ss-theme ss-theme-directory))))
    (f-write
     (mustache-render
      (f-read (concat (ss-get-theme-template-dir ss-theme ss-theme-directory)
                      "layout.mustache"))
      (ht ("page-title" ss-site-title)
          ("site-title" ss-site-title)
          ("site-sub-title" ss-site-sub-title)
          ("content" (ss--render-index-content file-tlist))
          ("keywords" ss-site-main-keywords)
          ("description" ss-site-main-desc)
          ("author" ss-author)
          ("item-count" (length file-tlist))))
     'utf-8
     (concat ss-dist-directory "/index.html"))))

(defun ss--render-index-content (file-tlist)
  "Render index content based on FILE-TLIST."
  (mustache-render
   (f-read (concat (ss-get-theme-template-dir ss-theme ss-theme-directory)
                   "index.mustache"))
   (ht ("file-tlist" file-tlist))))

(provide 'ss-index)

;;; ss-index.el ends here
