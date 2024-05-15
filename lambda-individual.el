;;; lambda-individual.el --- Individual info of Emacs config
;; Time-stamp: <2024-05-15 20:48:19 Guanghui Xu>

;;; Commentary:

;; When using this Emacs configuration, add this file to setup indivudual things.

;;; Code:

(setq user-full-name "Guanghui Xu"
      user-mail-address "gh_xu@qq.com"
      ;; source-directory "/home/xgh/sources/emacs-24.5"
      org-directory "/Users/hudandan/Documents/gtd"
      org-agenda-files (list
                        ;; (concat org-directory "/inbox.org")
                        (concat org-directory "/gtd.org")
                        ;; (concat org-directory "/someday.org")
                        ;; (concat org-directory "/tickler.org")
                        (concat org-directory "/capture.org"))

      org-default-notes-file (concat org-directory "/capture.org")

      ;; org-refile-targets '(("/Users/hudandan/Documents/gtd/gtd.org" :maxlevel . 3))
      )

(require 'shell-window)
(shell-window-mode 1)
(diminish 'shell-window-mode)
(define-key shell-window-mode-map (kbd "C-c s") #'shell-window-switch-buffer)
(define-key shell-window-mode-map (kbd "C-l") #'shell-window-clear-shell-buffer)

(provide 'lambda-individual)

;;; lambda-individual.el ends here
