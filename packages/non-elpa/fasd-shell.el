;;; fasd-shell.el --- Use ido completion to cd to already visited directories (and more).


;; Copyright (C) 2014 vindarel

;; Author: vindarel <ehvince@mailz.org>
;; Keywords: cli shell-mode bash zsh autojump
;; Version: 20140929
;; X-Original-Version: DEV
;; URL: https://gitlab.com/emacs-stuff/fasd-shell/tree/master

;; This file is not part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;; Package-Requires: ((s "1.9.0"))

;;; Commentary:

;; In shell mode, make the TAB key trigger ido completion to change directory.
;; Type "d  ema lisp RET" to have ido asking what directory whose full path matches "ema" and "lisp" to cd to.

;;; Requirements:
;; the `fasd' command line tool, see: https://github.com/clvv/fasd

;;; Usage:

;; (require 'fasd-shell)
;; (add-hook 'shell-mode-hook 'fasd-shell-mode)

;;; Code:

(require 's)

(defgroup fasd-shell nil
  "Quickly cd to previously-visited directories in shell mode, with ido-completion."
  :group 'tools
  :group 'convenience)

(defun fasd-get-path-list (pattern)
  "Call fasd with the given pattern and return the list of possibilities."
  (s-split "\n" (s-trim (shell-command-to-string (format "fasd -l -R %s" pattern))))
)

(defun fasd ()
  "If current shell command is `d something' then call fasd."
  (interactive)
  (let* ((user-input (buffer-substring-no-properties (comint-line-beginning-position)
                                                     (point-max))))
    (if (and (string= (substring user-input 0 2) "d "))  ;; todo: mapping to use something else than d and change directory.
        (progn
          ;; get what is after "d "
          (setq fasd-pattern (buffer-substring-no-properties (+ (comint-line-beginning-position) 2) (point-max)))
          (setq fasd-command (concat "cd " (ido-completing-read "cd to: " (fasd-get-path-list fasd-pattern))))
          (comint-kill-input)
          (insert fasd-command)
          (comint-send-input)
          ))
    ;; trigger the normal TAB completion:
    ;; (yas-expand)
    (completion-at-point)
    ))


(define-minor-mode fasd-shell-mode
  "Toggle fasd completion in shell mode."
  :global t
  :group 'fasd-shell

  (if fasd-shell-mode
      ;; Use TAB as in a normal shell. Now we have even better completion than zsh !
      (define-key shell-mode-map (kbd "<tab>") 'fasd)
    (define-key shell-mode-map (kbd "<tab>") 'completion-at-point)
))

(provide 'fasd-shell)
;;; fasd-shell ends here
