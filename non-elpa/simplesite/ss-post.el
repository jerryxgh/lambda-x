;;; ss-post.el --- Generate post page -*- lexical-binding: t -*-

;; Copyright 2015 Jerry Xu
;;
;; Author: Jerry Xu gh_xu@qq.com
;; Maintainer: Jerry Xu gh_xu@qq.com
;; Version: 0.0
;; Keywords: org-mode, blog, static site, html
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

(require 'f)
(require 'mustache)

(require 'ss-options)
(require 'ss-theme)

(defun ss-generate-post (post-table)
  "Generate post based on POST-TABLE."
  (ht-set post-table "page-title" (concat (ht-get post-table "title")
                                          " - "
                                          ss-site-title))
  (ht-set post-table "site-title" ss-site-title)
  (ht-set post-table "site-sub-title" ss-site-sub-title)
  (ht-set post-table "author" ss-author)
  (ht-set post-table "content" (ss-render-post-content post-table))
  (let ((mustache-partial-paths (list (ss-get-theme-template-dir)))
        (output-dir (ht-get post-table "output-dir")))
    (if (not (file-directory-p output-dir))
        (mkdir output-dir t))
    (f-write
     (mustache-render
      (f-read (concat (ss-get-theme-template-dir) "layout.mustache"))
      post-table)
     'utf-8
     (concat output-dir "index.html"))))

(defun ss-render-post-content (post-table)
  "Render content component of post based on POST-TABLE."
  (mustache-render
   (f-read (concat (ss-get-theme-template-dir) "post.mustache"))
   post-table))

(provide 'ss-post)

;;; ss-post.el ends here
