;;; lambda-blog.el --- org, confluence wiki
;; Time-stamp: <2015-10-28 16:35:23 Jerry Xu>
;;; Commentary:
;; blog about settings

;;; Code:

(require 'lambda-core)
(require 'org)

;; org-mode
;; align chinese font in org table, solution is from below:
;; http://baohaojun.github.io/blog/2012/12/19/perfect-emacs-chinese-font.html
(setq scalable-fonts-allowed t)

(setq org-use-speed-commands t
      org-directory "/home/xgh/文档/"
      org-src-fontify-natively t
      org-src-tab-acts-natively t)
(setq org-agenda-files (list "/home/xgh/文档/gtd.org"
                             (concat org-directory "/capture.org")))

(setq org-default-notes-file (concat org-directory "/capture.org"))
(global-set-key (kbd "C-c c") 'org-capture)

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)


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

;; export org to pdf
;;git clone git://github.com/tsdye/org-article.git
;;cd org-article
;;emacs -batch --eval "(org-babel-tangle-file \"article-class.org\")"
;;sudo cp org-article.cls /usr/share/texlive/texmf-dist/tex/latex/base/
;;sudo mktexlsr
;;kpsewhich org-article.cls
(require 'ox-latex)
(setq org-latex-listings t)
(add-to-list 'org-latex-classes
             '("org-article"
               "\\documentclass{org-article}
                 [NO-DEFAULT-PACKAGES]
                 [EXTRA]"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(setq org-latex-pdf-process
      '("xelatex -interaction nonstopmode %b"
        "xelatex -interaction nonstopmode %b"))


(add-to-list 'load-path "/home/xgh/repository/simplesite/")
(require 'simplesite)
(require 'ss-options)
(setq ss-source-directory
      "/home/xgh/repository/jerryxgh.github.io/source"
      ss-author "Jerry")


(provide 'lambda-blog)

;;; lambda-blog.el ends here
