;;; lambda-writing.el --- org, confluence wiki
;; Time-stamp: <2014-04-14 13:41:21 Jerry Xu>
;;; Commentary:
;; core settings

;;; Code:

(require 'lambda-core)

;; org -------------------------------------------------------------------------
(setq org-use-speed-commands t)

(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)

(lambda-package-ensure-install 'htmlize)
(defadvice htmlize-buffer-1 (around ome-htmlize-buffer-1 disable)
  "Rainbow-delimiters-mode has some problems with htmlize, this advice disable\
rainbow-delimiters-mode temporarily when using htmlize."
  (rainbow-delimiters-mode -1)
  ad-do-it
  (rainbow-delimiters-mode t))

(ad-enable-advice 'htmlize-buffer-1 'around 'ome-htmlize-buffer-1)
        (ad-activate 'htmlize-buffer-1)

(provide 'lambda-writing)
;;; lambda-writing.el ends here
