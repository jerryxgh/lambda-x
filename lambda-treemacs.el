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

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                2000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")
          treemacs-hide-dot-git-directory          nil
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-project-follow-into-home        nil
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           50
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil

          treemacs-persist-file                    (expand-file-name "treemacs-persist" lambda-auto-save-dir)
          treemacs-last-error-persist-file         (expand-file-name "treemacs-persist-at-last-error" lambda-auto-save-dir)
          )

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
  :after (treemacs evil)
  :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-all-the-icons
  :ensure t
  :after (treemacs)
  ;; should run all-the-icons-install-fonts after installation
  :config (treemacs-load-theme "all-the-icons"))


(use-package dired-subtree
  :ensure t
  :bind (:map dired-mode-map
              ("<tab>" . dired-subtree-cycle)
              ("TAB" . dired-subtree-cycle))
  :config
  (require 'dired-subtree)
  ;; (defun treemacs-icons-after-subtree-insert-a ()
  ;;   (if (> (line-number-at-pos) 1)
  ;;       (let ((ov (dired-subtree--get-ov)))
  ;;         (cl-letf (((symbol-function 'eobp)
  ;;                    (lambda ()
  ;;                      (when ov
  ;;                        (<= (overlay-end ov) (point))))))
  ;;           (treemacs-icons-dired--reset)
  ;;           (treemacs-icons-dired--display-icons-for-subdir (dired-current-directory) (point))))))
  ;; (advice-add 'dired-subtree-insert :after #'treemacs-icons-after-subtree-insert-a)

  (defun treemacs-icons-after-subtree-insert-hook ()
    (let ((pos (point))
          (end (overlay-end (dired-subtree--get-ov))))
      (treemacs-with-writable-buffer
         (save-excursion
           (goto-char pos)
           (dired-goto-next-file)
           (treemacs-block
            (while (< (point) end)
              (if (dired-move-to-filename nil)
                  (let* ((file (dired-get-filename nil t))
                         (icon (if (file-directory-p file)
                                   (treemacs-icon-for-dir file 'closed)
                                 (treemacs-icon-for-file file))))
                    (insert icon))
                (treemacs-return nil))
              (forward-line 1)))))))
  (add-hook 'dired-subtree-after-insert-hook 'treemacs-icons-after-subtree-insert-hook))

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
  :after (treemacs persp-mode) ;;or perspective vs. persp-mode
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))

(defun lsp-treemacs-generic-update (tree)
  (setq lsp-treemacs-tree tree)
  (lsp-treemacs-generic-refresh))

(provide 'lambda-treemacs)

;;; lambda-treemacs.el ends here
