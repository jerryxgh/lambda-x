;;; ss-tags.el --- generate tags pages -*- lexical-binding: t -*-

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
;; Generate tag files.


;;; Code:

(require 'ss-options)
(require 'ss-theme)
(require 'color)

(defun ss-generate-tags (file-tlist)
  "Generate tags pages based on FILE-TLIST.

FILE-TLIST: hash table of all source files"
  (let ((tag-list (ss--parse-tags file-tlist)))
    (ss--generate-tags-index tag-list)
    (mapc #'(lambda (e)
              (ss--generate-tag-page e))
          tag-list)))

(defun ss--generate-tags-index (tag-list)
  "Generate tags index page based on TAG-LIST.

TAG-LIST: hash table of <tag, file>."
  (let ((mustache-partial-paths
         (list (ss-get-theme-template-dir ss-theme ss-theme-directory)))
        (output-dir (concat ss-dist-directory "/tags")))
    (if (not (file-directory-p output-dir))
        (mkdir output-dir t))
    (f-write
     (mustache-render
      (f-read (concat (ss-get-theme-template-dir ss-theme ss-theme-directory)
                      "layout.mustache"))
      (ht ("page-title" (concat "All tags - " ss-site-title))
          ("site-title" ss-site-title)
          ("site-sub-title" ss-site-sub-title)
          ("content" (ss--render-tags-index-content tag-list))
          ("keywords" ss-site-main-keywords)
          ("description" ss-site-main-desc)
          ("author" ss-author)))
     'utf-8
     (concat output-dir "/index.html")))
  )

(defun ss--generate-tag-page (tag-table)
  "Generate tag page based on TAG-TABLE."
  (let* ((mustache-partial-paths
          (list (ss-get-theme-template-dir ss-theme ss-theme-directory)))
         (tag-name (ht-get tag-table "name"))
         (output-dir (concat ss-dist-directory "/tags/" tag-name)))
    (if (not (file-directory-p output-dir))
        (mkdir output-dir t))
    (f-write
     (mustache-render
      (f-read (concat (ss-get-theme-template-dir ss-theme ss-theme-directory)
                      "layout.mustache"))
      (ht ("page-title" (concat "Tags: "
                                tag-name " - " ss-site-title))
          ("site-title" ss-site-title)
          ("site-sub-title" ss-site-sub-title)
          ("content" (ss--render-tag-page-content tag-table))
          ("keywords" ss-site-main-keywords)
          ("description" ss-site-main-desc)
          ("author" ss-author)))
     'utf-8
     (concat output-dir "/index.html"))))

(defun ss--render-tags-index-content (tag-list)
  "Render tags index content based on TAG-LIST."
  (mustache-render
   (f-read (concat (ss-get-theme-template-dir ss-theme ss-theme-directory)
                   "tags-index.mustache"))
   (ht ("tags" tag-list)
       ("all-count" (length tag-list)))))

(defun ss--render-tag-page-content (tag-table)
  "Render tag page content based on TAG-TABLE."
  (mustache-render
   (f-read (concat (ss-get-theme-template-dir ss-theme ss-theme-directory)
                   "tag.mustache"))
   tag-table))

(defun ss--parse-tags (file-tlist)
  "Convert FILE-TLIST to hash table of <tag, file>."
  (let ((tag-table (ht-create))
        (tag-max-count 1)
        tag-color-gradient
        tag-list)
    (mapc #'(lambda (e)
              (let ((tags (ht-get e "tags")))
                (if (not tags)
                    (ht-set tag-table
                            "no-tag"
                            (cons e (ht-get tag-table "no-tag")))
                  (mapc #'(lambda (tag)
                            (ht-set tag-table
                                    tag
                                    (cons e (ht-get tag-table tag))))
                        tags))))
          (reverse file-tlist))
    ;; convert tag-table from hashtable to list and initialize tag-max-count
    (setq tag-list
          (ht-map #'(lambda (key value)
                      (let ((tag-count (length value)))
                        (if (> tag-count tag-max-count)
                            (setq tag-max-count tag-count))
                        (ht ("name" key)
                            ("uri" (concat "/tags/" key))
                            ("files" value)
                            ("count" tag-count))))
                  tag-table))

    ;; set tag font size and color in tag cloud
    (setq tag-color-gradient (ss--compute-tag-color-gradient tag-max-count))
    (mapc #'(lambda (e)
              (let ((tag-count (ht-get e "count")))
                (ht-set e "font-size" (ss--compute-tag-font-size tag-count
                                                                 tag-max-count))
                (ht-set e "color" (nth (- tag-count 1) tag-color-gradient))))
          tag-list)

    ;; sort by tag name
    (setq tag-list
          (sort tag-list
                #'(lambda (a b)
                    (string< (ht-get a "name")
                             (ht-get b "name")))))
    tag-list))

(defun ss--compute-tag-font-size (count max-count)
  "Compute font size of a tag with COUNT posts.

MAX-COUNT: maximal posts that all tags have."
  (if (< max-count 1)
      ss-tag-cloud-min-font
    (+ (round (* (/ (- count 1.0) (- max-count 1.0))
                 (- ss-tag-cloud-max-font ss-tag-cloud-min-font)))
       ss-tag-cloud-min-font)))

(defun ss--compute-tag-color-gradient (max-count)
  "Return a list with MAX-COUNT colors from `ss-tag-cloud-start-color' to \
`ss-tag-cloud-end-color'."
  (mapcar #'(lambda (color)
              (apply #'color-rgb-to-hex color))
          (cons (color-name-to-rgb ss-tag-cloud-start-color)
                (append
                 (color-gradient (color-name-to-rgb ss-tag-cloud-start-color)
                                 (color-name-to-rgb ss-tag-cloud-end-color)
                                 (- max-count 2))
                 (list (color-name-to-rgb ss-tag-cloud-end-color))))))

(provide 'ss-tags)

;;; ss-tags.el ends here
