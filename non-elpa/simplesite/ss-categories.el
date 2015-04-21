;;; ss-categories.el --- generate categories pages -*- lexical-binding: t -*-

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

(require 'f)
(require 'mustache)

(require 'ss-options)
(require 'ss-theme)

(defun ss-generate-categories (file-tlist)
  "Generate categories pages based on FILE-TLIST.

FILE-TLIST: hash table of all source files"
  (let ((cate-list (ss-parse-categories file-tlist)))
    (ss-generate-categories-index cate-list)
    (mapc #'(lambda (e)
              (ss-generate-category-page e))
          cate-list)))

(defun ss-generate-categories-index (cate-list)
  "Generate categories index page baaed on CATE-LIST.

CATE-LIST: hash table of <category, file>."
  (let ((mustache-partial-paths (list (ss-get-theme-template-dir)))
        (output-dir (concat ss-dist-directory "/categories")))
    (if (not (file-directory-p output-dir))
        (mkdir output-dir t))
    (f-write
     (mustache-render
      (f-read (concat (ss-get-theme-template-dir) "layout.mustache"))
      (ht ("page-title" (concat "All categories - " ss-site-title))
          ("site-title" ss-site-title)
          ("site-sub-title" ss-site-sub-title)
          ("content" (ss-render-categories-index-content cate-list))
          ("keywords" ss-site-main-keywords)
          ("description" ss-site-main-desc)
          ("author" ss-author)))
     'utf-8
     (concat output-dir "/index.html"))))

(defun ss-generate-category-page (category-table)
  "Generate one category page based CATEGORY-TABLE."
  (let* ((mustache-partial-paths (list (ss-get-theme-template-dir)))
         (cate-name (ht-get category-table "name"))
         (output-dir (concat ss-dist-directory "/categories/" cate-name)))
    (if (not (file-directory-p output-dir))
        (mkdir output-dir t))
    (f-write
     (mustache-render
      (f-read (concat (ss-get-theme-template-dir) "layout.mustache"))
      (ht ("page-title" (concat "Categories: "
                                cate-name " - " ss-site-title))
          ("site-title" ss-site-title)
          ("site-sub-title" ss-site-sub-title)
          ("content" (ss-render-category-page-content category-table))
          ("keywords" ss-site-main-keywords)
          ("description" ss-site-main-desc)
          ("author" ss-author)))
     'utf-8
     (concat output-dir "/index.html"))))

(defun ss-render-category-page-content (category)
  "Render category page content based on CATEGORY."
  (mustache-render
   (f-read (concat (ss-get-theme-template-dir) "category.mustache"))
   category))

(defun ss-render-categories-index-content (cate-list)
  "Render categories index content based on CATE-LIST."
  (mustache-render
   (f-read (concat (ss-get-theme-template-dir) "categories-index.mustache"))
   (ht ("categories" cate-list)
       ("all-count" (length cate-list)))))

(defun ss-parse-categories (file-tlist)
  "Parse categories based on FILE-TLIST."
  (let ((category-table (ht-create))
        categoriy-list)
    (mapc #'(lambda (e)
              (let ((category (ht-get e "category")))
                (ht-set category-table
                        category
                        (cons e (ht-get category-table category)))))
          file-tlist)
    ;; convert hashtable to list
    (setq categoriy-list
          (ht-map #'(lambda (key value)
                      (ht ("name" key)
                          ("uri" (concat "/categories/" key))
                          ("files" value)
                          ("count" (length value))))
                  category-table))
    ;; sort by category name
    (setq categoriy-list
          (sort categoriy-list
                #'(lambda (a b)
                    (string< (ht-get a "name")
                             (ht-get b "name")))))
    categoriy-list))

(provide 'ss-categories)

;;; ss-categories.el ends here
