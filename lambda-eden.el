;;; lambda-eden.el --- A testing place -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'lambda-core)

;;; TODO
;; evil-nerd-commenter
;; elnode emacs server
;; 研究 prelude purcell elpy srecode-mode, improve lambda-x according to prelude.

(add-hook 'c-mode-common-hook #'(lambda ()
                                  (hs-minor-mode)
                                  (diminish 'hs-minor-mode)))
;; vlf - view large file -------------------------------------------------------
(lambda-package-ensure-install 'vlf)
(require 'vlf-setup)
(custom-set-variables '(vlf-application 'dont-ask))
;; iedit -----------------------------------------------------------------------
(lambda-package-ensure-install 'iedit)

;; ztree - compare directories -------------------------------------------------
(lambda-package-ensure-install 'ztree)
;; (push (substitute-in-file-name "path-to-ztree-directory") load-path)
;; (require 'ztree-dir)

(defun view-time (time-seconds)
  "Convert TIME-SECONDS from the epoch (0:00 January 1, 1970 UTC) to time string."
  (current-time-string (seconds-to-time time-seconds)))

;; ====================
;; insert date or time
(defvar current-time-format "%T"
  "Format of date to insert with `insert-current-time' func.
Note the weekly scope of the command's precision.")

(defun insert-current-date ()
  "Insert current date in iso 8601 format into current buffer."
  (interactive)
  (insert (concat
           (format-time-string "%Y-%m-%d"))))

(defun insert-current-date-time ()
  "Insert current date and time in iso 8601 format into current buffer."
  (interactive)
  (insert (concat
           (format-time-string "%Y-%m-%dT%T")
           (funcall (lambda (x)
                      (concat (substring x 0 3) ":" (substring x 3 5)))
                    (format-time-string "%z")))))

(defun insert-current-time ()
  "Insert the current time (1-week scope) into the current buffer."
  (interactive)
  (insert (format-time-string current-time-format (current-time) t)))

;; (global-set-key "\C-c\C-d" 'insert-current-date-time)
;; (global-set-key "\C-c\C-t" 'insert-current-time)

(defun lambda-copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

;;; es-mode
;; (lambda-package-ensure-install 'es-mode)

;; template engine
(lambda-package-ensure-install 'mustache)
(lambda-package-ensure-install 'f)

(lambda-package-ensure-install 'nginx-mode)
;; (require 'nginx-mode)
(add-to-list 'auto-mode-alist
             '("modsecurity\.conf$" . nginx-mode))

;; to try
;; Comment-dwim-2 evil-nerd-commenter chinese-fonts-?
;; default-text-scale font-utils fontawesome zap-to-char zlc
;; good
(lambda-package-ensure-install 'git-messenger)
;; (require 'git-messenger)
;; (setq git-messenger:show-detail t)

(lambda-package-ensure-install 'zoom-frm)

;; A-x string-edit-at-point "C-c C-c" "C-c C-k"
(lambda-package-ensure-install 'string-edit)

;; trying
(lambda-package-ensure-install 'ac-ispell)
(lambda-package-ensure-install 'ace-link)
(lambda-package-ensure-install 'adaptive-wrap)
(lambda-package-ensure-install 'aggressive-indent)
(lambda-package-ensure-install 'auto-highlight-symbol)
(lambda-package-ensure-install 'bind-key)
(lambda-package-ensure-install 'clean-aindent-mode)
(lambda-package-ensure-install 'eval-sexp-fu)
(lambda-package-ensure-install 'evil-args)
(lambda-package-ensure-install 'evil-nerd-commenter)
(lambda-package-ensure-install 'expand-region)
(lambda-package-ensure-install 'git-timemachine)
(lambda-package-ensure-install 'gitattributes-mode)
(lambda-package-ensure-install 'gitconfig-mode)
(lambda-package-ensure-install 'gitignore-mode)
(lambda-package-ensure-install 'golden-ratio)
;; (lambda-package-ensure-install 'guide-key)
;; (require 'guide-key)
;; (setq guide-key/popup-window-position 'bottom
;;       guide-key/guide-key-sequence t)
;; (guide-key-mode 1) ; Enable guide-key-mode
;; (diminish 'guide-key-mode)

(lambda-package-ensure-install 'helm-c-yasnippet)
(lambda-package-ensure-install 'helm-descbinds)
(lambda-package-ensure-install 'helm-gitignore)
(lambda-package-ensure-install 'helm-mode-manager)
(lambda-package-ensure-install 'helm-orgcard)
(lambda-package-ensure-install 'helm-swoop)
(lambda-package-ensure-install 'highlight)
(lambda-package-ensure-install 'highlight-indentation)
(lambda-package-ensure-install 'highlight-numbers)
(lambda-package-ensure-install 'highlight-parentheses)
(lambda-package-ensure-install 'hl-anything)
(lambda-package-ensure-install 'hungry-delete)
(global-hungry-delete-mode 1)
(diminish 'hungry-delete-mode)
(lambda-package-ensure-install 'ibuffer-projectile)
(lambda-package-ensure-install 'indent-guide)
(lambda-package-ensure-install 'info+)
(lambda-package-ensure-install 'linum-relative)
(lambda-package-ensure-install 'move-text)

(lambda-package-ensure-install 'org-bullets)
;; (require 'org-bullets)
(add-hook 'org-mode-hook #'(lambda () (org-bullets-mode 1)))
(lambda-package-ensure-install 'org-present)
(lambda-package-ensure-install 'org-repo-todo)
(lambda-package-ensure-install 'page-break-lines)
(lambda-package-ensure-install 'paradox)
(lambda-package-ensure-install 'rfringe)
(lambda-package-ensure-install 'csv-mode)
(lambda-package-ensure-install 'evil-textobj-anyblock)
(lambda-package-ensure-install 'expand-line)
(lambda-package-ensure-install 'multi-term)

;; (lambda-package-ensure-install 'company)
;; (lambda-package-ensure-install 'company-dict)
;; (lambda-package-ensure-install 'company-web)
;; (lambda-package-ensure-install 'company-tern)
;; (lambda-package-ensure-install 'company-auctex)
;; (lambda-package-ensure-install 'company-math)
;; (require 'company)
;; (require 'company-dabbrev-code)

;; Add yasnippet support for all company backends
;; https://github.com/syl20bnr/spacemacs/pull/179
;; (defvar company-mode/enable-yas t
;;   "Enable yasnippet for all backends.")

;; (defun company-mode/backend-with-yas (backend)
;;   "Let yasnippt work with other company BACKEND."
;;   (if (or (not company-mode/enable-yas)
;;           (and (listp backend) (member 'company-yasnippet backend)))
;;       backend
;;     (append (if (consp backend) backend (list backend))
;;             '(:with company-yasnippet))))

;; (add-to-list 'company-backends 'company-cmake t)
;; (add-to-list 'company-backends 'company-eclim t)

;; (define-key company-mode-map (kbd "M-/") 'company-complete)
;; (define-key company-active-map (kbd "C-s") 'company-filter-candidates)
;; (define-key company-active-map (kbd "C-M-s") 'company-search-candidates)
;; (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)

;; (setq company-backends
;;       (mapcar #'company-mode/backend-with-yas company-backends)

;;       company-auto-complete t
;;       company-dabbrev-code-everywhere t
;;       company-minimum-prefix-length 2)


;; (add-hook 'after-init-hook #'(lambda ()
;;                                (global-company-mode 1)))

;; (define-key ac-completing-map (kbd "<tab>") 'ac-expand)
;; (define-key ac-completing-map (kbd "<backtab>") 'ac-previous)
;; (define-key ac-completing-map (kbd "<return>") 'ac-complete)

;; web servers
(lambda-package-ensure-install 'simple-httpd)
;; (lambda-package-ensure-install 'elnode)
;; (lambda-package-ensure-install 'web-server)

;; org and chinese
;; cal-china-x chinese-fonts-setup org-chinese-utils org-latex-chinese pangu-spacing
(lambda-package-ensure-install 'ox-latex-chinese)

;; knowledge
;; locate-library
;; list-load-path-shadows

;;; svn - psvn =================================================================
(lambda-package-ensure-install 'psvn)
(setq svn-status-hide-unmodified t)
(lambda-package-ensure-install 'magit-svn)
(add-hook 'magit-mode-hook 'magit-svn-mode)

;;; rfc  =======================================================================
(lambda-package-ensure-install 'irfc)
(setq irfc-directory "/Users/xgh/repository/docs/rfc")
(setq irfc-assoc-mode t)
(add-to-list 'auto-mode-alist '("/rfc[0-9]+\\.txt\\'" . irfc-mode))

;; yasnippet ===================================================================
;; (lambda-package-ensure-install 'auto-yasnippet)
;; (lambda-package-ensure-install 'language-detection)

;; fasd ========================================================================
(require 'fasd-shell)
(add-hook 'shell-mode-hook 'fasd-shell-mode)

;; whitespace-clean-mode =======================================================
(lambda-package-ensure-install 'whitespace-cleanup-mode)
(global-whitespace-cleanup-mode 1)

(provide 'lambda-eden)

;;; lambda-eden.el ends here
