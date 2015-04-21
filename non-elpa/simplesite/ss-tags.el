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

(require 'f)
(require 'mustache)

(require 'ss-options)
(require 'ss-theme)

(defun ss-generate-tags (file-tlist)
  "Generate tags pages based on FILE-TLIST.

FILE-TLIST: hash table of all source files"
  )

(defun ss-generate-tags-index (tag-list)
  "Generate tags index page based on TAG-LIST.

TAG-LIST: hash table of <tag, file>."

  )

(defun ss-generate-tags-page (tag-table)
  "Generate tag page based on TAG-TABLE.")

(defun ss-render-tag-index-content (tag-list)
  "Render tags index content based on TAG-LIST.")

(defun ss-render-tag-page-content (tag)
  "Render tag page content based on TAG.")

(defun ss-parse-tags (file-tlist)
  "Convert FILE-TLIST to hash table of <tag, file>."
  (let ((tag-table (ht-create))
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
          file-tlist)
    ;; convert tag-table from hashtable to list
    (setq tag-list
          (ht-map #'(lambda (key value)
                      (ht ("name" key)
                          ("uri" (concat "/tags/" key))
                          ("files" value)
                          ("count" (length value))))
                  tag-table))

    ;; sort by tag name
    (setq tag-list
          (sort tag-list
                #'(lambda (a b)
                    (string< (ht-get a "name")
                             (ht-get b "name")))))
    tag-list))

(provide 'ss-tags)

;;; ss-tags.el ends here
