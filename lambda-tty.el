;;; lambda-tty.el --- Make TTY Emacs suck less -*- lexical-binding: t -*-

;; Copyright (C) 2024 Guanghui Xu
;;
;; Author: Guanghui Xu gh_xu@qq.com
;; Maintainer: Guanghui Xu gh_xu@qq.com
;; Created: 2024-05-20
;; Version:
;; Keywords:
;; Homepage: not distributed yet
;; Package-Version:
;; Package-Requires:
;;

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Reference doc:https://docs.doomemacs.org/v21.12/modules/os/tty/

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'lambda-tty)

;;; Change Log:

;; Version $(3) 2024-05-20 GuanghuiXu
;;   - Initial release

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
