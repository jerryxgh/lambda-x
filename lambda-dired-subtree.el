;;; lambda-dired-subtree.el ---  -*- lexical-binding: t -*-

;; Time-stamp: <2025-02-17 15:09:17 Guanghui Xu>

;;; Commentary:
;; Configuration for dired-subtree.

;;; Code:

(require 'lambda-treemacs)

;; redefine this function to avoid double directory icon when revert-buffer in dired-mode.
(defun treemacs-icons-dired--display-icons-for-subdir (path pos)
  "Display icons for subdir PATH at given POS."
  (unless (member path treemacs-icons-dired--covered-subdirs)
    (add-to-list 'treemacs-icons-dired--covered-subdirs path)
    (treemacs-with-writable-buffer
     (save-excursion
       (goto-char pos)
       (dired-goto-next-file)
       (treemacs-block
        (while (not (eobp))
          (if (dired-move-to-filename nil)
              (let* ((file (dired-get-filename nil t))
                     (icon (if (file-directory-p file)
                               (treemacs-icon-for-dir file 'closed)
                             (treemacs-icon-for-file file))))
                (if (file-directory-p file)
                    (if (not (string-suffix-p icon (buffer-substring (line-beginning-position) (point))))
                        (insert icon))
                  (insert icon)))
            (treemacs-return nil))
          (forward-line 1)))))))


;; when open dire-subtree, insert icons
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

(use-package dired-subtree
  :ensure t
  :bind (:map dired-mode-map
              ("<tab>" . dired-subtree-cycle)
              ("TAB" . dired-subtree-cycle))
  :config
  (if (display-graphic-p)
      ;; when open dire-subtree, insert icons
      (add-hook 'dired-subtree-after-insert-hook 'treemacs-icons-after-subtree-insert-hook)))
(require 'dired-subtree)

(use-package evil-collection
  :ensure t
  :custom
  ;; minibuffer use emacs default key bindings
  ;; (evil-collection-setup-minibuffer t)
  (evil-collection-outline-enable-in-minor-mode-p nil)
  (evil-collection-mode-list (evil-filter-list (lambda (item) (member item '(comint company)))
                                               evil-collection--supported-modes))
  :config
  (evil-collection-init))

(provide 'lambda-dired-subtree)

;;; lambda-dired-subtree.el ends here
