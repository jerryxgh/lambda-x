;;; lambda-blog.el --- org, confluence wiki
;; Time-stamp: <2015-04-04 14:47:55 Jerry Xu>
;;; Commentary:
;; core settings

;;; Code:

(require 'lambda-core)

;; org-mode
(setq org-use-speed-commands t)

(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)

;; htmlize --------------------------------------------------------------------
(lambda-package-ensure-install 'htmlize)
(defadvice htmlize-buffer-1 (around ome-htmlize-buffer-1 disable)
  "Rainbow-delimiters-mode has some problems with htmlize, this advice disable\
rainbow-delimiters-mode temporarily when using htmlize."
  (rainbow-delimiters-mode -1)
  ad-do-it
  (rainbow-delimiters-mode t))

(ad-enable-advice 'htmlize-buffer-1 'around 'ome-htmlize-buffer-1)
        (ad-activate 'htmlize-buffer-1)

;; org-page --------------------------------------------------------------------
(lambda-package-ensure-install 'org-page)
(require 'org-page)
(setq op/repository-directory "/home/xgh/repository/jerryxgh.github.io")
(setq op/site-domain "http://jerryxgh.github.io")
;; for commenting, you can choose either disqus or duoshuo
(setq op/personal-disqus-shortname "jerryxgh")
;; (setq op/personal-duoshuo-shortname "your_duoshuo_shortname")
;; the configuration below are optional
;; (setq op/personal-google-analytics-id "your_google_analytics_id")
(setq op/theme 'simple)

;;(require 'org-publish)
;;
;;(setq org-publish-project-alist
;;      '(
;;        ;; path to org file
;;        ("blog-org"
;;         :base-directory "~/repository/jerryxgh.github.com/org/"
;;         :base-extension "org"
;;
;;         ;; path to jekyll project
;;         :publishing-directory "~/repository/jerryxgh.github.com/"
;;         :recursive t
;;         :publishing-function org-publish-org-to-html
;;         :headline-levels 4
;;         :html-extension "html"
;;         :section-numbers nil
;;         :auto-preamble t
;;         :body-only t ; only export section between <body> </body>
;;
;;         ;; generate sitemap.org automagically
;;         :auto-sitemap t
;;         :sitemap-filename "sitemap.org"
;;         :sitemap-title "Sitemap"
;;
;;         :author "jerryxgh"
;;         :email "gh_xu@qq.com"
;;         :style    ""
;;         )
;;
;;        ("blog-static"
;;         :base-directory "~/repository/jerryxgh.github.com/org/"
;;         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
;;         :publishing-directory "~/repository/jerryxgh.github.com/"
;;         :recursive t
;;         :publishing-function org-publish-attachment
;;         )
;;
;;        ("blog" :components ("blog-org" "blog-static"))))

(provide 'lambda-blog)

;;; lambda-blog.el ends here
