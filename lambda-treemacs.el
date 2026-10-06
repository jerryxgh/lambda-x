;;; lambda-treemacs.el --- for treemacs -*- lexical-binding: t -*-

;; Copyright (C) 2021 Guanghui Xu
;;
;; Author: Guanghui Xu gh_xu@qq.com
;; Maintainer: Guanghui Xu gh_xu@qq.com
;; Created: 2021-11-03

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'lambda-treemacs)

;;; Change Log:

;; Version $(3) 2021-11-03 GuanghuiXu
;;   - Initial release

;;; Code:

(require 'lambda-core)
(require 'lambda-evil)

(use-package treemacs
  :ensure t
  ;; :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    ;; Keep only intentional overrides; use the built-in icon theme on all frames.
    (setq treemacs-width 30
          treemacs-read-string-input 'from-minibuffer
          treemacs-is-never-other-window nil
          treemacs-no-delete-other-windows t
          treemacs-show-hidden-files t
          treemacs-hide-dot-git-directory t
          treemacs-persist-file
          (expand-file-name "treemacs-persist" lambda-auto-save-dir)
          treemacs-last-error-persist-file
          (expand-file-name "treemacs-persist-at-last-error" lambda-auto-save-dir))

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;; (treemacs-resize-icons 18)

    ;; beautify treemacs mode line
    (setq treemacs-user-mode-line-format
          (progn
            (spaceline-compile
              "treemacs" '(((persp-name workspace-number "➓")
                            :fallback evil-state :face highlight-face :priority 100)
                           (anzu :priority 95)
                           (major-mode :priority 79))
              `(which-function
                (buffer-position :priority 99)
                (hud :priority 100)))
            '("%e" (:eval (spaceline-ml-treemacs)))))

    (treemacs-follow-mode t)
    ;; (treemacs-tag-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (treemacs-git-commit-diff-mode t)

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  ;; :after (treemacs evil)
  :ensure t)

(use-package treemacs-projectile
  ;; :after (treemacs projectile)
  :ensure t)

;; Dired stays text-only: subtree expansion must not insert duplicate icons.
;; Treemacs itself selects GUI/TTY icons from its built-in theme.

(use-package treemacs-magit
  ;; :after (treemacs magit)
  :ensure t)

(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
  ;; :after (treemacs persp-mode) ;;or perspective vs. persp-mode
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))

(provide 'lambda-treemacs)

;;; lambda-treemacs.el ends here
