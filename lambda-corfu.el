;;; lambda-corfu.el --- Completion in GUI and terminal frames -*- lexical-binding: t -*-

;;; Commentary:
;; Keep fallback completion sources behind each major mode's own CAPFs.

;;; Code:

(require 'lambda-core)

(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 2)
  :config
  (corfu-history-mode 1)
  (global-corfu-mode 1))

;; Emacs 31 has native TTY child frames.  Older versions need Popon.
;; Install the maintained package instead of shadowing it with a vendored copy.
(unless (featurep 'tty-child-frames)
  (use-package corfu-terminal
    :ensure t
    :after corfu
    :no-require t
    :commands corfu-terminal-mode
    :init
    (corfu-terminal-mode 1)))

(setq completion-cycle-threshold 3
      tab-always-indent 'complete)

(use-package dabbrev
  :ensure nil
  :config
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  (dolist (mode '(authinfo-mode doc-view-mode pdf-view-mode tags-table-mode))
    (add-to-list 'dabbrev-ignored-buffer-modes mode)))

(defun lambda-cape-setup ()
  "Append a small set of buffer-local fallback completion functions."
  (dolist (function '(cape-file cape-dabbrev))
    (add-hook 'completion-at-point-functions function t t)))

(use-package cape
  :ensure t
  :bind (("M-/" . cape-dabbrev)
         ("C-M-/" . dabbrev-expand)
         ("C-c /" . cape-prefix-map))
  :hook ((prog-mode . lambda-cape-setup)
         (text-mode . lambda-cape-setup)
         (shell-mode . lambda-cape-setup)
         (eshell-mode . lambda-cape-setup)
         (eglot-managed-mode . lambda-cape-setup)))

;; Text labels also render in TTY frames and do not require SVG support.
(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-use-icons nil)
  (kind-icon-blend-background t)
  (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(provide 'lambda-corfu)
;;; lambda-corfu.el ends here
