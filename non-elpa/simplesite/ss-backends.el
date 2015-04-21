;;; ss-backends.el --- For parsing source files -*- lexical-binding: t -*-

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
(require 'ht)
(require 'ss-org-backend)

(defun ss-parse-all-src-files (src-dir dist-dir)
  "Parse all src files to hashmap list, return sorted list.

SRC-DIR: directory to hold all source files.
DIST-DIR: directory to hold all generated files."
  (let ((file-tlist
         (mapcar #'(lambda (src-file)
                     (ss-export-org-file src-file
                                         src-dir
                                         dist-dir
                                         (ss-file-changed-p src-file)))
                 (ss-get-all-src-files src-dir))))
    ;; delete nil elements
    (setq file-tlist (-filter #'(lambda (e) e) file-tlist))
    ;; sort files by date in descendent order
    (setq file-tlist (sort file-tlist
                           #'(lambda (a b)
                               (string< (ht-get b "date")
                                        (ht-get a "date")))))
    ;; set previous post and next post for every post
    (let ((next-post (car file-tlist)))
      (mapc #'(lambda (e)
                (ht-set e "next-post" next-post)
                (ht-set next-post "prev-post" e)
                (setq next-post e))
            (cdr file-tlist)))
    file-tlist))

(defun ss-get-all-src-files (directory)
  "Return all source files under DIRECTORY and it's subdirectory.

TODO: make regexp configurable."
  (ss-walk-directory directory "^.*\\.org$"))

(defun ss-walk-directory
  (&optional directory match ignore-directories)
  "Walk through DIRECTORY tree.
If DIRECTOR is nil, use `default-directory' as startpoint.
Argument MATCH can be a predicate or a regexp.
Argument IGNORE-DIRECTORIES can be list of file names to be ignored."
  (unless directory
    (setq directory default-directory))
  (unless (file-directory-p directory)
    (setq directory (file-name-directory directory)))
  (let (directory-stack result)
    (if (file-exists-p directory)
        (push directory directory-stack))
    (while directory-stack
      (let* ((current-directory (pop directory-stack))
             (file-list (directory-files current-directory t)))
        (mapc #'(lambda (file)
                  (if (not (file-symlink-p file))
                      (let ((file-name (file-name-nondirectory file)))
                        (if (file-directory-p file)
                            (if (and (not (equal file-name "."))
                                     (not (equal file-name ".."))
                                     (not (member file-name ignore-directories)))
                                (push file directory-stack))
                          (if match
                              (and (if (functionp match)
                                       (funcall match file)
                                     (and (stringp match)
                                          (string-match match file-name)))
                                   (push file result))
                            (push file result))))))
              file-list)))
    result))

(defun ss-file-changed-p (file)
  "Judge a FILE is changed or not by it's md5 value.

TODO: not implemented."
  (if file
      t))

(provide 'ss-backends)

;;; ss-backends.el ends here
