;;; ss-org-backend.el --- Export org files to html files -*- lexical-binding: t -*-

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

;; Export org files to html files

;;; Code:

(require 'ox)

(require 'dash)
(require 'f)
(require 's)
(require 'ht)

(defun ss-export-org-file (org-file src-dir dist-dir should-generate-content-p)
  "Export one ORG-FILE, if it is changed, return file attributes.

SRC-DIR should be `ss-source-directory',
DIST-DIR should be `ss-dist-directory',
but to be simple, try to not use global variables.
If SHOULD-GENERATE-CONTENT-P is t, generate content of ORG-FLLE, else, just get
attributes of ORG-FILE, but not generate content of it."
  (let ((output-dir (ss-get-output-dir org-file dist-dir src-dir))
        (uri (ss-get-uri org-file src-dir))
        attr-table)
    (with-temp-buffer
      (setq buffer-file-coding-system 'utf-8)
      (insert-file-contents org-file)

      ;; collect attributes
      (setq attr-table
            (ht ("file" org-file)
                ("md5" (ss-md5-file org-file))
                ("title" (or (ss-get-org-option "TITLE") "Untitled"))
                ("author" (or (ss-get-org-option "AUTHOR")
                              user-full-name "Unknown Author"))
                ("description" (or (ss-get-org-option "DESCRIPTION")
                                   "No Description"))
                ("keywords" (or (ss-get-org-option "TAGS") ""))
                ("category" (or (ss-get-category org-file src-dir)
                                "default"))
                ("uri" uri)
                ("date" (or (ss-get-org-option "DATE")
                            (ss-format-iso-8601-date
                             (nth 5 (file-attributes org-file)))))
                ("email" user-mail-address)
                ("output-dir" output-dir)))
      (let ((tags (ht-get attr-table "keywords")))
        (if tags
            (ht-set attr-table
                    "tags"
                    (delete "" (mapcar 'trim-string
                                       (split-string tags "[:,]+" t))))))

      (if should-generate-content-p
          (ht-set attr-table "post-content" (org-export-as 'html nil nil t nil))))

    attr-table))

(defun ss-get-output-dir (org-file dist-dir src-dir)
  "Get output directory of ORG-FILE, which ends with /.

Result = DIST-DIR + \"/posts/\" + (ORG-FILE - SRC-DIR) - suffix + /."
  (concat
   (file-name-sans-extension
    (concat dist-dir "/posts/" (s-chop-prefix src-dir org-file)))
   "/"))

(defun ss-get-uri (org-file src-dir)
  "Get uri of ORG-FILE.

Result = \"/posts/\" + (ORG-FILE - SRC-DIR - suffix)."
  (concat "/posts/" (file-name-sans-extension (s-chop-prefix src-dir org-file))))

(defun ss-get-category (org-file src-dir)
  "Get category of ORG-FILE.

Result = first directory of (ORG-FILE - SRC-DIR), if nil, return filename."
  (let ((src-dir (directory-file-name src-dir)))
    (while (not (string= src-dir (f-parent org-file)))
      (setq org-file (f-parent org-file)))
    (file-name-base org-file)))

(defun ss-get-org-option (option)
  "Read option value of org file opened in current buffer.
e.g:
#+TITLE: this is title
will return \"this is title\" if OPTION is \"TITLE\""
  (let ((match-regexp (org-make-options-regexp `(,option))))
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward match-regexp nil t)
          (match-string-no-properties 2 nil)))))

(defun ss-format-iso-8601-date (date)
  "Format DATE to iso-8601 format."
  (concat
   (format-time-string "%Y-%m-%d" date)))

(defun ss-format-iso-8601-time (time)
  "Format TIME to iso-8601 format."
  (concat
   (format-time-string "%Y-%m-%dT%T" time)
   (funcall (lambda (x) (concat (substring x 0 3) ":" (substring x 3 5)))
            (format-time-string "%z" time))))

(defun ss-md5-file (file)
  "Return md5 digest of FILE."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (setq buffer-file-coding-system 'binary)
    (insert-file-contents-literally file)
    (md5 (current-buffer))))

(provide 'ss-org-backend)

;;; ss-org-backend.el ends here
