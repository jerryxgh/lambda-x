;;; ss-theme.el --- Process theme things -*- lexical-binding: t -*-

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

(defun ss-prepare-theme (theme theme-dir dist-dir load-dir)
  "Copy resources of THEME in THEME-DIR to DIST-DIR.

If THEME not exist in THEME-DIR, use default theme `next' under LOAD-DIR."
  (let ((theme-resource-dist-dir (expand-file-name "resources/" dist-dir))
        (theme-resource-dir (ss-get-theme-resource-dir theme theme-dir)))
    (unless (file-directory-p theme-resource-dir)
      (message "Theme %s not found, use default theme `next' instead." theme)
      (setq theme-dir (concat load-dir"themes/"))
      (setq theme-resource-dir (ss-get-theme-resource-dir theme theme-dir)))
    (if (file-directory-p theme-resource-dist-dir)
        (delete-directory theme-resource-dist-dir t))
    (copy-directory theme-resource-dir theme-resource-dist-dir t t t)))

(defun ss-get-theme-resource-dir (theme theme-dir)
  "Return resource directory of THEME under THEME-DIR ending with /."
  (file-name-as-directory
   (expand-file-name
    (format "%s/resources" theme) theme-dir)))

(defun ss-get-theme-template-dir (theme theme-dir)
  "Return theme template directory of THEME under THEME-DIR ending with /."
  (file-name-as-directory
   (expand-file-name
    (format "%s/templates" theme) theme-dir)))

(provide 'ss-theme)

;;; ss-theme.el ends here
