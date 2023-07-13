;;; lambda-ivy.el --- ivy configs -*- lexical-binding: t -*-

;;; Commentary:
;; ivy configs

;;; Code:

(require 'lambda-core)

;; Ivy re-uses the following packages if they are installed: avy, amx or smex, flx, and wgrep.
;; for fuzzy matching sorting
(use-package flx
  :ensure t)

(use-package avy
  :ensure t)

;; show most-used commands in the completion list and showing keyboard shortcuts
(use-package amx
  :ensure t
  :custom
  ;; There are two features that can cause a noticeable delay (around 1/4 of a
  ;; second) when running amx: command ignoring and showing key
  ;; bindings. Generally this delay is not a problem, since you can start typing
  ;; imediately and Amx will catch up after the short delay. However, if it
  ;; bothers you, these delays can be greatly reduced by setting one or both of
  ;; amx-ignored-command-matchers and amx-show-key-bindings to nil.
  (amx-ignored-command-matchers nil)
  (amx-show-key-bindings nil)
  :init (amx-mode 1))

;; ivy ======================================================================
(use-package ivy
  :ensure t
  :diminish (ivy-mode . "")
  :bind
  (:map ivy-mode-map
   ("C-'" . ivy-avy))
  :config
  (ivy-mode 1)
  ;; add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’.
  (setq enable-recursive-minibuffers t
        ivy-use-virtual-buffers t)
  ;; number of result lines to display
  (setq ivy-height 10)
  ;; does not count candidates
  (setq ivy-count-format "")
  ;; no regexp by default
  (setq ivy-initial-inputs-alist nil)
  ;; configure regexp engine.
  (setq ivy-re-builders-alist
        ;; allow input not in order
        '(
          (t . ivy--regex-fuzzy)
          ;; (t . ivy--regex-ignore-order)
          ))
  ;; incompatible with ivy
  (if (and (bound-and-true-p hungry-delete-except-modes)
           (not (member 'minibuffer-mode hungry-delete-except-modes)))
      (add-to-list 'hungry-delete-except-modes 'minibuffer-mode))
  ;; Use C-j for immediate termination with the current value, and RET for
  ;; continuing completion for that directory. This is the ido behaviour.
  (define-key ivy-minibuffer-map (kbd "C-j") #'ivy-immediate-done)
  (define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done))

(use-package counsel
  :ensure t
  :bind (("C-s" . swiper))
  :config
  ;; Don't show "." and ".." in counsel-find-file
  (setq ivy-extra-directories nil)
  (setq counsel-find-file-at-point t))

(use-package counsel-projectile
  :ensure t
  :config
  (counsel-projectile-mode))

(global-set-key (kbd "C-x j") (lambda ()
                                (interactive)
                                (counsel-find-file lambda-package-direcotry)))
(provide 'lambda-ivy)
;;; lambda-ivy.el ends here
