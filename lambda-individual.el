;;; lambda-individual.el --- Individual info of Emacs config
;; Time-stamp: <2019-05-11 10:57:24 Guanghui Xu>

;;; Commentary:

;; When using this Emacs configuration, add this file to setup indivudual things.

;;; Code:

(setq user-full-name "Guanghui Xu"
      user-mail-address "gh_xu@qq.com"
      ;; source-directory "/home/xgh/sources/emacs-24.5"
      org-directory "/Users/hudandan/Documents/gtd"
      org-agenda-files (list
                        ;; (concat org-directory "/inbox.org")
                        (concat org-directory "/gtd.org")
                        ;; (concat org-directory "/someday.org")
                        ;; (concat org-directory "/tickler.org")
                        (concat org-directory "/capture.org"))

      org-default-notes-file (concat org-directory "/capture.org")

      ;; org-refile-targets '(("/Users/hudandan/Documents/gtd/gtd.org" :maxlevel . 3))
      )

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

;; set font for myself

(when (eq system-type 'gnu/linux)
  (set-frame-font "Consolas-11.5")
  (if (fboundp 'set-fontset-font)
      (set-fontset-font t 'unicode '("Microsoft Yahei" .
                                     "unicode-bmp")))
  (setq face-font-rescale-alist (list (cons "Microsoft Yahei" 1.1))))

(provide 'lambda-individual)

;;; lambda-individual.el ends here
