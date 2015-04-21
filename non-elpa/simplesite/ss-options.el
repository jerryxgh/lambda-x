;;; ss-options.el --- All options of simplesite -*- lexical-binding: t -*-

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

;; Options of simplesite.

;;; Code:

(require 'ox)
(require 'ht)

(defgroup simplesite nil
  "Options for generating static site."
  :tag "Org static page generator" :group 'org)

(defconst ss-temp-buffer-name "*simplesite output*"
  "Name of the temporary buffer used by simplesite.")

(defconst ss-load-directory
  (cond
   (load-file-name (file-name-directory load-file-name))
   ((symbol-file 'ss-temp-buffer-name)
    (file-name-directory (symbol-file 'ss-temp-buffer-name)))
   ((string= (file-name-nondirectory buffer-file-name) "ss-options.el")
    (file-name-directory buffer-file-name))
   (t nil))
  "Directory where simple-site is loaded from.")

(defcustom ss-source-directory nil
  "Directory of all source files for generating site."
  :group 'simplesite :type 'string)

(defcustom ss-author nil
  "Directory of all source files for generating site."
  :group 'simplesite :type 'string)

(defcustom ss-dist-directory nil
  "Directory for generated files."
  :group 'simplesite :type 'string)

(defcustom ss-site-domain nil
  "Domain name of entire site.
It is recommended to assign with prefix http:// or https://,  http will be
considered if not assigned."
  :group 'simplesite :type 'string)

(defcustom ss-site-title "simplesite"
  "Main title of entire site."
  :group 'simplesite :type 'string)

(defcustom ss-site-main-desc "description"
  "Main description of entire site."
  :group 'simplesite :type 'string)

(defcustom ss-site-main-keywords "keywords"
  "Main keywords of entire site."
  :group 'simplesite :type 'string)

(defcustom ss-site-sub-title "static site generator"
  "Subtitle of entire site."
  :group 'simplesite :type 'string)

(defcustom ss-theme-directory
  (concat ss-load-directory "themes/")
  "Directory stores themes for page rendering.
By default, it points to the directory `themes' in simplesite installation
directory."
  :group 'simplesite :type 'string)

(defcustom ss-theme 'next
  "Theme used for page generation."
  :group 'simplesite :type 'symbol)

(defcustom ss-personal-github-link nil
  "Personal github link."
  :group 'simplesite :type 'string)

(defcustom ss-personal-avatar nil
  "Link to an avatar image."
  :group 'simplesite :type 'string)

(defcustom ss-personal-disqus-shortname nil
  "Personal disqus shortname."
  :group 'simplesite :type 'string)

(defcustom ss-personal-duoshuo-shortname nil
  "Personal duoshuo shortname."
  :group 'simplesite :type 'string)

(defcustom ss-personal-google-analytics-id nil
  "Personal google analytics id."
  :group 'simplesite :type 'string)

(defcustom ss-confound-email t
  "This is used to determine whether email should be confounded or not."
  :group 'simplesite :type 'boolean)

(defcustom ss-html-creator-string
  (format "<a href=\"http://www.gnu.org/software/emacs/\">Emacs</a> %s\
 (<a href=\"http://orgmode.org\">Org mode</a> %s)"
          (format "%s.x" emacs-major-version)
          (if (fboundp 'org-version)
              (replace-regexp-in-string "\\..*" ".x" (org-version))
            "Unknown Version"))
  "Information about the creator of the HTML document."
  :group 'simplesite
  :type 'string)

(defconst ss-rss-template "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<rss version=\"2.0\">
  <channel>
    <title>{{title}}</title>
    <link>{{link}}</link>
    <description>{{description}}</description>
    <pubDate>{{date}}</pubDate>
    <lastBuildDate>{{date}}</lastBuildDate>
    <docs>http://www.rssboard.org/rss-specification</docs>
    {{#items}}
    <item>
      <title>{{item-title}}</title>
      <link>{{item-link}}</link>
      <description>{{item-description}}</description>
      <pubDate>{{item-update-date}}</pubDate>
      <guid>{{item-link}}</guid>
    </item>
    {{/items}}
  </channel>
</rss>"
  "Template for RSS rendering.")

(provide 'ss-options)

;;; ss-options.el ends here
