;;; lambda-blog.el --- org
;; Time-stamp: <2016-08-16 16:30:29 Guanghui Xu>
;;; Commentary:
;; blog about settings

;;; Code:

(require 'lambda-core)
(require 'org)
(require 'org-id)

;; org-mode
;; align chinese font in org table, solution is from below:
;; http://baohaojun.github.io/blog/2012/12/19/perfect-emacs-chinese-font.html
(setq scalable-fonts-allowed t)

(setq org-use-speed-commands t
      org-src-fontify-natively t
      org-completion-use-ido t
      org-id-locations-file
      (expand-file-name ".org-id-locations" lambda-auto-save-dir)
      org-src-tab-acts-natively t)

(global-set-key (kbd "C-c c") 'org-capture)

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)

;; markdown -------------------------------------------------------------------
(lambda-package-ensure-install 'markdown-mode)

;; htmlize --------------------------------------------------------------------
(lambda-package-ensure-install 'htmlize)
(defadvice htmlize-buffer-1 (around ome-htmlize-buffer-1 disable)
  "Rainbow-delimiters-mode has some problems with htmlize, this advice disable\
rainbow-delimiters-mode temporarily when using htmlize."
  (rainbow-delimiters-mode-disable)
  ad-do-it
  (rainbow-delimiters-mode-enable))

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

;; latex editing ---------------------------------------------------------------
(lambda-package-ensure-install 'auctex)
(lambda-package-ensure-install 'auto-complete-auctex)


;; graphviz-dot-mode - mode for editing dot files ------------------------------
(lambda-package-ensure-install 'graphviz-dot-mode)
(setq graphviz-dot-indent-width 4)

(provide 'lambda-blog)

;;; lambda-blog.el ends here
