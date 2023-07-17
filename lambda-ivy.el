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
  :ensure t
  :config
  (when (and (featurep 'evil) (featurep 'evil-leader))
    (evil-leader/set-key
      "c" 'avy-goto-char
      "w" 'avy-goto-word-0
      "l" 'avy-goto-line)))

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
  :custom
  ;; number of result lines to display
  (ivy-height 10)
  ;; does not count candidates
  ;; (ivy-count-format "")
  ;; no regexp by default
  (ivy-initial-inputs-alist nil)
  ;; add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’.
  (enable-recursive-minibuffers t)
  (ivy-use-virtual-buffers t)

  :bind
  (;; Use C-j for immediate termination with the current value, and RET for
   ;; continuing completion for that directory. This is the ido behaviour.
   (:map ivy-minibuffer-map
         ("C-j" . ivy-immediate-done)
         ("RET" . ivy-alt-done)))
  :config
  ;; configure regexp engine.
  (setq ivy-re-builders-alist
        '((counsel-ag . ivy--regex)
          ;; allow input not in order
          ;; (counsel-ag . ivy--regex-ignore-order)
          (swiper . ivy--regex)
          (t . ivy--regex-fuzzy)))
  (ivy-mode 1)
  ;; hungry-delete-mode is incompatible with ivy in minibuffer-mode
  (if (and (bound-and-true-p hungry-delete-except-modes)
           (not (member 'minibuffer-mode hungry-delete-except-modes)))
      (add-to-list 'hungry-delete-except-modes 'minibuffer-mode)))

(use-package counsel
  :ensure t
  :bind (("C-s" . swiper))
  :delight counsel-mode
  :config
  (counsel-mode 1)
  ;; Don't show '.' and '..' in counsel-find-file
  (setq ivy-extra-directories nil)
  (setq counsel-find-file-at-point t)
  (defun lambda-counsel-ag ()
    (interactive)
    (counsel-ag nil default-directory)))

(use-package ivy-xref
  :ensure t
  :init
  ;; xref initialization is different in Emacs 27 - there are two different
  ;; variables which can be set rather than just one
  (when (>= emacs-major-version 27)
    (setq xref-show-definitions-function 'ivy-xref-show-defs))
  ;; Necessary in Emacs <27. In Emacs 27 it will affect all xref-based
  ;; commands other than xref-find-definitions (e.g. project-find-regexp)
  ;; as well
  (setq xref-show-xrefs-function 'ivy-xref-show-xrefs))

(use-package counsel-projectile
  :ensure t
  :config
  (counsel-projectile-mode))

(use-package ivy-prescient
  :ensure t
  :config
  (ivy-prescient-mode 1))

(global-set-key (kbd "C-x j") (lambda ()
                                (interactive)
                                (counsel-find-file lambda-package-direcotry)))

(require 'flymake)
(require 'counsel)
(defun counsel-flymake (&optional buffer)
  "Show flymake errors of BUFFER in ivy.
BUFFER defaults to current buffer."
  (interactive)
  (let* (;; flymake--diagnostics-buffer-source is needed by (flymake--diagnostics-buffer-entries)
         (flymake--diagnostics-buffer-source (or buffer (current-buffer)))
         (file-name (let ((bfn (buffer-file-name flymake--diagnostics-buffer-source)))
                      (if bfn (file-name-base (file-name-sans-extension bfn))
                        (file-name-sans-extension (buffer-name flymake--diagnostics-buffer-source)))))
         (errors (flymake--diagnostics-buffer-entries))
         ;; for dolist result
         result
         (cands (reverse (dolist (err errors result)
                           (let* ((diag (plist-get (car err) :diagnostic))
                                  (vec (nth 1 err))
                                  (line (aref vec 0))
                                  (message (concat (aref vec 2) " " (car (aref vec 4)))))
                             (setq result (cons (propertize (concat file-name ":" line ":" message)
                                                            'point (flymake--diag-beg diag))
                                                result)))))))
    (if cands (counsel-mark--ivy-read "flymake errors: " cands 'counsel-flymake)
      (message "flymake errors: no errors"))))

(provide 'lambda-ivy)
;;; lambda-ivy.el ends here
