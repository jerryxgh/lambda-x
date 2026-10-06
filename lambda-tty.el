;;; lambda-tty.el --- Terminal frame integration -*- lexical-binding: t -*-

;;; Commentary:
;; Enable integrations independently of the initial frame's display type.

;;; Code:

(require 'lambda-core)
(require 'lambda-evil)

;; Clipetty checks the selected frame before emitting OSC 52 sequences.
(use-package clipetty
  :ensure t
  :no-require t
  :commands global-clipetty-mode)

(use-package evil-terminal-cursor-changer
  :ensure t
  :no-require t
  :commands evil-terminal-cursor-changer-activate)

(setq evil-motion-state-cursor 'box
      evil-visual-state-cursor 'box
      evil-normal-state-cursor 'box
      evil-insert-state-cursor 'bar
      evil-emacs-state-cursor 'hbar)

(defvar lambda-terminal-cursor-enabled nil
  "Non-nil after installing the terminal cursor integration.")

(defun lambda-configure-terminal-frame (frame)
  "Enable terminal cursor integration when a TTY FRAME is created."
  (when (and (not noninteractive)
             (not (display-graphic-p frame))
             (not lambda-terminal-cursor-enabled))
    (with-selected-frame frame
      (global-clipetty-mode 1)
      (evil-terminal-cursor-changer-activate)
      (setq lambda-terminal-cursor-enabled t))))

(add-hook 'after-make-frame-functions #'lambda-configure-terminal-frame)
(dolist (frame (frame-list))
  (lambda-configure-terminal-frame frame))

(defun lambda-term-keys-want-key-p-def (key mods)
  "Lambda implementation for `term-keys/want-key-p-func'.

This function controls which key combinations are to be encoded
and decoded using the term-keys protocol extension.
KEY is the KeySym name as listed in `term-keys/mapping'; MODS is
a 6-element bool vector representing the modifiers Shift /
Control / Meta / Super / Hyper / Alt respectively, with t or nil
representing whether they are depressed or not.  Returns non-nil
if the specified key combination should be encoded.

Note that the ALT modifier rarely actually corresponds to the Alt
key on PC keyboards; the META modifier will usually be used
instead."
  (and (elt mods 1)
       (not (elt mods 3))
       (not (elt mods 4))
       (not (elt mods 5))
       (member key '("Left" "Right"))))

;; use command below to generate alacritty config, convert to toml like this:
;; { key = "Left", mods = "Command", chars = "\u001b\u001f\u0054\u0062\u001f" },
;; { key = "Left", mods = "Command | Shift", chars = "\u001b\u001f\u0054\u0063\u001f" },
;; { key = "Right", mods = "Command", chars = "\u001b\u001f\u0055\u0042\u001f" },
;; { key = "Right", mods = "Command | Shift", chars = "\u001b\u001f\u0055\u0043\u001f" },

;; (require 'term-keys-alacritty)
;; (with-temp-buffer
;;   (insert (term-keys/alacritty-config))
;;   (write-region
;;    (point-min) (point-max) "~/alacritty-for-term-keys.yml"))

(use-package term-keys
  :ensure t
  :if (not noninteractive)
  :custom
  (term-keys/want-key-p-func 'lambda-term-keys-want-key-p-def)
  :config
  (term-keys-mode t))

(provide 'lambda-tty)
;;; lambda-tty.el ends here
