;; lambda-evil.el --- configuration for evil

;; Time-stamp: <2022-04-07 11:29:02 Guanghui Xu>

;;; Commentary:
;; Configuration for evil.

;;; Code:

;; evil ------- A wonderful editor in Emacs ------------------------------------
(lambda-package-ensure-install 'evil)
(setq-default evil-want-C-w-delete nil
              evil-want-visual-char-semi-exclusive t
              evil-want-C-w-in-emacs-state t
              evil-auto-balance-windows t
              evil-cross-lines t)

(require 'evil)

(evil-set-undo-system 'undo-tree)

;; treat underscore as part of the word
(with-eval-after-load 'evil
  (defalias #'forward-evil-word #'forward-evil-symbol))

(evil-mode 1)
;; let * and # search symbol instead of word at point
(setq-default evil-symbol-word-search t)
;; settings below restore key bindings in emacs in insert state
(define-key evil-insert-state-map (kbd "C-S-k") 'evil-insert-digraph)
(define-key evil-insert-state-map (kbd "C-a") 'move-beginning-of-line)
(define-key evil-insert-state-map (kbd "C-b") 'backward-char)
(define-key evil-insert-state-map (kbd "C-d") 'delete-char)
(define-key evil-insert-state-map (kbd "C-e") 'move-end-of-line)
(define-key evil-insert-state-map (kbd "C-f") 'forward-char)
(define-key evil-insert-state-map (kbd "C-k") 'kill-line)
(define-key evil-insert-state-map (kbd "C-n") 'next-line)
(define-key evil-insert-state-map (kbd "C-p") 'previous-line)
(define-key evil-insert-state-map (kbd "C-t") 'transpose-chars)
;; (define-key evil-insert-state-map (kbd "C-w") 'evil-window-map)
(define-key evil-insert-state-map (kbd "C-y") 'yank)

(define-key evil-normal-state-map (kbd "M-.") ())
(define-key evil-normal-state-map (kbd "C-t") ())
(when (eq system-type 'darwin)
  (define-key evil-insert-state-map (kbd "C-v") 'yank))


;; Prevent the visual selection overriding my system clipboard?

;; On some operating systems, there is only one clipboard for both copied and
;; selected texts. This has the consequence that visual selection – which should
;; normally be saved to the PRIMARY clipboard – overrides the SYSTEM clipboard,
;; where normally goes the copied text. This can be corrected by adding the
;; following code to the dotspacemacs/user-config of your .spacemacs:
(fset 'evil-visual-update-x-selection 'ignore)

(defun lambda-hs-hide-level-1 ()
  "Just fold level 1 elements."
  (hs-hide-level 1))
(define-key evil-normal-state-map (kbd "zM")
  '(lambda ()
     (interactive)
     (let ((hs-hide-all-non-comment-function 'lambda-hs-hide-level-1))
       (evil-close-folds))))

(defun lambda-copy-to-end-of-line ()
  "Copy to end of line, and bind this funciton to Y in normal mode."
  (interactive)
  (evil-yank (point) (point-at-eol)))
;; (define-key evil-normal-state-map (kbd "C-w") 'evil-window-map)
(define-key evil-normal-state-map (kbd "Y") 'lambda-copy-to-end-of-line)
(define-key evil-normal-state-map (kbd "g f") 'find-file-at-point)

(delete 'ag-mode evil-motion-state-modes)

(mapc #'(lambda (mode-state-pair)
          (evil-set-initial-state (car mode-state-pair) (cdr mode-state-pair)))
      '(
        (Info-mode . emacs)
        (Man-mode . emacs)
        (calendar-mode . emacs)
        (dired-mode . emacs)
        (grep-mode . emacs)
        (help-mode . emacs)
        (image-mode . emacs)
        (svn-status-mode . emacs)
        (view-mode . emacs)
        (xref . emacs)
        (special-mode . emacs)
        (ag-mode . emacs)
        ))

;; when entering edebug, change to evil-emacs-state to use simple key bindings
;; (require 'edebug)
;; (add-hook 'edebug-mode-hook (lambda ()
;;                               (if edebug-mode
;;                                   (evil-emacs-state)
;;                                 (evil-normal-state))))
;; (add-hook 'edebug-mode-hook 'evil-normalize-keymaps)

;;(define-key evil-motion-state-map (kbd "C-i") 'evil-jump-forward)
;; (define-key evil-emacs-state-map (kbd "C-w") 'evil-window-map)

;; (require 'evil-tab-minor-mode)
;; (global-evil-tab-mode t)

;; evil-leader -----------------------------------------------------------------
(lambda-package-ensure-install 'evil-leader)
(require 'evil-leader)
(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
  "a" 'helm-ag
  "p" 'helm-projectile-ag
  ;; "b" #'(lambda ()
  ;;         (interactive)
  ;;         ;; skip persp-mode like filters, let it show more candidates
  ;;         (let ((ido-make-buffer-list-hook nil))
  ;;           (ido-switch-buffer)))
  "b" 'ido-switch-buffer
  "e" 'helm-projectile
  "k" 'kill-this-buffer
  "o" 'helm-occur
  "f" 'find-file)
(global-evil-leader-mode 1)

(lambda-package-ensure-install 'evil-nerd-commenter)
;; (evil-leader/set-key
;;   "ci" 'evilnc-comment-or-uncomment-lines
;;   "cl" 'evilnc-quick-comment-or-uncomment-to-the-line
;;   "ll" 'evilnc-quick-comment-or-uncomment-to-the-line
;;   "cc" 'evilnc-copy-and-comment-lines
;;   "cp" 'evilnc-comment-or-uncomment-paragraphs
;;   "cr" 'comment-or-uncomment-region
;;   "cv" 'evilnc-toggle-invert-comment-line-by-line
;;   "."  'evilnc-copy-and-comment-operator
;;   "\\" 'evilnc-comment-operator ; if you prefer backslash key
;; )

;; evil-exchange ---------------------------------------------------------------
;; powerful tool to exchange text
;; gx (evil-exchange)
;; gX (evil-exchange-cancel)
;; evil-exchange can be used with ace-jump, it's perfect
(lambda-package-ensure-install 'evil-exchange)
(require 'evil-exchange)
(evil-exchange-install)

;; evil-matchit ----------------------------------------------------------------
;; Jump between beginning and ending of structure like parens, html tags etc..
(lambda-package-ensure-install 'evil-matchit)
(require 'evil-matchit)
(global-evil-matchit-mode 1)

;; evil-visualstar -------------------------------------------------------------
(lambda-package-ensure-install 'evil-visualstar)
(require 'evil-visualstar)
(global-evil-visualstar-mode t)

;; expand-region ---------------------------------------------------------------
(lambda-package-ensure-install 'expand-region)
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
(if (featurep 'evil-leader)
    (progn
      (setq expand-region-contract-fast-key "z")
      (evil-leader/set-key "x" 'er/expand-region)))

;; evil-surround ---------------------------------------------------------------
;; add surrounding
;; visual-state: S<textobject><trigger>, normal-state: ys<textobject><trigger>.

;; change surrounding
;; cs<old-trigger><new-trigger>

;; delete surrounding
;; ds<trigger>.
(lambda-package-ensure-install 'evil-surround)
(require 'evil-surround)
(global-evil-surround-mode 1)

;; ace jump --------------------------------------------------------------------
(lambda-package-ensure-install 'ace-jump-mode)
(require 'ace-jump-mode)

(when (and (featurep 'evil) (featurep 'evil-leader))
  (evil-leader/set-key
    "c" 'ace-jump-char-mode
    "w" 'ace-jump-word-mode
    "l" 'ace-jump-line-mode))
(lambda-package-ensure-install 'ace-window)
(global-set-key (kbd "C-x o") 'ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
(define-key evil-window-map (kbd "w") 'ace-window)
(define-key evil-window-map (kbd "C-w") 'ace-window)

(lambda-package-ensure-install 'evil-commentary)
(require 'evil-commentary)
(evil-commentary-mode)
(diminish 'evil-commentary-mode)

(lambda-package-ensure-install 'evil-numbers)
(require 'evil-numbers)
(define-key evil-normal-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt)
(define-key evil-visual-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt)
(define-key evil-visual-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt)

;; anzu for evil ---------------------------------------------------------------
;; support for * or # search command
(lambda-package-ensure-install 'evil-anzu)
(require 'evil-anzu)

;; define text objects
(defmacro spacemacs|define-text-object (key name start end)
  "Define a text object and a surround pair.
START and END are strings (not regular expressions) that define
the boundaries of the text object."
  `(progn
     (spacemacs|define-text-object-regexp ,key ,name
                                          ,(regexp-quote start)
                                          ,(regexp-quote end))
     (with-eval-after-load 'evil-surround
       (push (cons (string-to-char ,key)
                   (if ,end
                       (cons ,start ,end)
                     ,start))
             evil-surround-pairs-alist))))

(defmacro spacemacs|define-text-object-regexp (key name start-regexp end-regexp)
  "Define a text object.
START-REGEXP and END-REGEXP are the boundaries of the text object."
  (let ((inner-name (make-symbol (concat "evil-inner-" name)))
        (outer-name (make-symbol (concat "evil-outer-" name))))
    `(progn
       (evil-define-text-object ,inner-name (count &optional beg end type)
         (evil-select-paren ,start-regexp ,end-regexp beg end type count nil))
       (evil-define-text-object ,outer-name (count &optional beg end type)
         (evil-select-paren ,start-regexp ,end-regexp beg end type count t))
       (define-key evil-inner-text-objects-map ,key (quote ,inner-name))
       (define-key evil-outer-text-objects-map ,key (quote ,outer-name)))))

(spacemacs|define-text-object "$" "dollar" "$" "$")
(spacemacs|define-text-object "*" "star" "*" "*")
(spacemacs|define-text-object "8" "block-star" "/*" "*/")
(spacemacs|define-text-object "|" "bar" "|" "|")
(spacemacs|define-text-object "%" "percent" "%" "%")
(spacemacs|define-text-object "/" "slash" "/" "/")
(spacemacs|define-text-object "_" "underscore" "_" "_")
(spacemacs|define-text-object "-" "hyphen" "-" "-")
(spacemacs|define-text-object "~" "tilde" "~" "~")
(spacemacs|define-text-object "=" "equal" "=" "=")

(evil-define-text-object evil-inner-buffer (count &optional beg end type)
  (list (point-min) (point-max)))
(define-key evil-inner-text-objects-map "g" 'evil-inner-buffer)

(lambda-package-ensure-install 'evil-smartparens)
(add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)

(provide 'lambda-evil)

;;; lambda-evil.el ends here
