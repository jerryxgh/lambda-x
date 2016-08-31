;;; lambda-individual.el --- Individual info of Emacs config
;; Time-stamp: <2016-08-22 19:56:39 Guanghui Xu>

;;; Commentary:

;; When using this Emacs configuration, add this file to setup indivudual things.

;;; Code:

(setq user-full-name "Guanghui Xu"
      user-mail-address "gh_xu@qq.com"
      source-directory "/home/xgh/sources/emacs-24.5"
      org-directory "/Users/xgh/repository/docs"
      org-agenda-files (list(concat org-directory "/gtd.org")
                            (concat org-directory "/capture.org"))

      org-default-notes-file (concat org-directory "/capture.org")
      sbt:program-name "/home/xgh/local/sbt/bin/sbt"
      )

;; configuring eclipse installation
(when (eq system-type 'gnu/linux)
  (add-to-list 'eclim-eclipse-dirs
               "/home/xgh/local/sts-bundle/sts-3.7.3.RELEASE/")
  (setq eclim-executable "/home/xgh/local/sts-bundle/sts-3.7.3.RELEASE/eclim"
        eclimd-executable "/home/xgh/local/sts-bundle/sts-3.7.3.RELEASE/eclimd"
        eclimd-default-workspace "~/workspace/"))

;; (add-to-list 'load-path "/home/xgh/repository/smartwin")
(lambda-package-ensure-install #'smartwin)
(require 'smartwin)
(smartwin-mode 1)
(diminish 'smartwin-mode)
(define-key smartwin-mode-map (kbd "C-c s") #'smartwin-switch-buffer)
(define-key smartwin-mode-map (kbd "C-w o") #'smartwin-enlarge)
(define-key smartwin-mode-map (kbd "C-l") #'smartwin-clear-shell)

;; (add-to-list 'load-path "~/repository/simplesite")
;; (require 'simplesite)
;; (setq simplesite-author "Guanghui Xu"
;;       simplesite-personal-avatar "/home/xgh/repository/jerryxgh.github.io/posts/Heckert_GNU_white.png"
;;       simplesite-source-directory "/home/xgh/repository/jerryxgh.github.io/posts"
;;       simplesite-output-directory "/home/xgh/repository/jerryxgh.github.io"
;;       simplesite-personal-github-link "http://jerryxgh.github.io"
;;       simplesite-site-domain "http://jerryxgh.github.io"

;;       simplesite-log-level 'SIMPLESITE-LOG-DEBUG

;;       simplesite-personal-disqus-shortname "jerryxgh"
;;       simplesite-personal-duoshuo-shortname nil)

(provide 'lambda-individual)

;;; lambda-individual.el ends here
