;;; lambda-tty.el --- Make TTY Emacs suck less -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'lambda-core)
(require 'lambda-evil)

(unless (display-graphic-p)
  (use-package clipetty
    :ensure t
    :hook (after-init . global-clipetty-mode))

  (use-package evil-terminal-cursor-changer
    :ensure t
    :config
    (require 'evil-terminal-cursor-changer)
    (evil-terminal-cursor-changer-activate) ; or (etcc-on)
    (setq evil-motion-state-cursor 'box)  ; █
    (setq evil-visual-state-cursor 'box)  ; █
    (setq evil-normal-state-cursor 'box)  ; █
    (setq evil-insert-state-cursor 'bar)  ; ⎸
    (setq evil-emacs-state-cursor  'hbar) ; _
    ))

(provide 'lambda-tty)

;;; lambda-tty.el ends here
