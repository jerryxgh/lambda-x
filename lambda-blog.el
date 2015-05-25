;;; lambda-blog.el --- org, confluence wiki
;; Time-stamp: <2015-05-20 13:32:56 Jerry Xu>
;;; Commentary:
;; core settings

;;; Code:

(require 'lambda-core)

;; org-mode
;; align chinese font in org table, solution is from below:
;; http://baohaojun.github.io/blog/2012/12/19/perfect-emacs-chinese-font.html
(setq scalable-fonts-allowed t)

(setq org-use-speed-commands t)

(setq org-agenda-files (list "/home/xgh/文档/gtd.org"))

(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)


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
