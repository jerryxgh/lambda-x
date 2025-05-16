;; lambda-evil.el --- configuration for evil

;; Time-stamp: <2025-05-16 19:25:03 Guanghui Xu>

;;; Commentary:
;; Configuration for evil.

;;; Code:

;; evil ------- A wonderful editor in Emacs ------------------------------------

(require 'lambda-core)

(use-package evil
  :ensure t
  :custom
  (evil-want-integration t) ;; This is optional since it's already set to t by default.
  (evil-want-keybinding nil)
  (evil-want-C-w-delete nil)
  (evil-want-visual-char-semi-exclusive t)
  (evil-want-C-w-in-emacs-state t)
  (evil-cross-lines t)
  (evil-symbol-word-search t) ;; let * and # search symbol instead of word at point
  (evil-want-Y-yank-to-eol t)
  (evil-want-change-word-to-end nil)

  :config
  (evil-set-undo-system 'undo-tree)
  ;; treat underscore as part of the word
  (defalias #'forward-evil-word #'forward-evil-symbol)

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
  (define-key evil-insert-state-map (kbd "M-.") 'evil-goto-definition)
  (define-key evil-insert-state-map (kbd "C-y") 'yank)

  (when (eq system-type 'darwin)
    (define-key evil-insert-state-map (kbd "C-v") 'yank))

  ;; (define-key evil-normal-state-map (kbd "Y") 'lambda-copy-to-end-of-line)
  (define-key evil-normal-state-map (kbd "g f") 'find-file-at-point)
  (define-key evil-normal-state-map (kbd "M-.") 'evil-goto-definition)

  (delete 'ag-mode evil-motion-state-modes)

  (mapc #'(lambda (mode-state-pair)
            (evil-set-initial-state (car mode-state-pair) (cdr mode-state-pair)))
        '(
          ;; (Info-mode . emacs)
          ;; (Man-mode . emacs)
          ;; (calendar-mode . emacs)
          ;; (dired-mode . emacs)
          ;; (grep-mode . emacs)
          ;; (help-mode . emacs)
          ;; (image-mode . emacs)
          ;; (svn-status-mode . emacs)
          ;; (view-mode . emacs)
          ;; (xref . emacs)
          ;; (special-mode . emacs)
          ;; (ag-mode . emacs)
          ;; (data-debug-mode . emacs)
          ;; (messages-buffer-mode . normal)
          ))

  ;; Prevent the visual selection overriding my system clipboard?
  ;; On some operating systems, there is only one clipboard for both copied and
  ;; selected texts. This has the consequence that visual selection – which should
  ;; normally be saved to the PRIMARY clipboard – overrides the SYSTEM clipboard,
  ;; where normally goes the copied text. This can be corrected by adding the
  ;; following code to the dotspacemacs/user-config of your .spacemacs:
  (fset 'evil-visual-update-x-selection 'ignore)

  (add-hook 'evil-local-mode-hook
            (lambda ()
              ;; Note:
              ;; Check if `company-emulation-alist' is in
              ;; `emulation-mode-map-alists', if true, call
              ;; `company-ensure-emulation-alist' to ensure
              ;; `company-emulation-alist' is the first item of
              ;; `emulation-mode-map-alists', thus has a higher
              ;; priority than keymaps of evil-mode.
              ;; We raise the priority of company-mode keymaps
              ;; unconditionally even when completion is not
              ;; activated. This should not cause problems,
              ;; because when completion is activated, the value of
              ;; `company-emulation-alist' is ((t . company-my-keymap)),
              ;; when completion is not activated, the value is ((t . nil)).
              (when (memq 'company-emulation-alist emulation-mode-map-alists)
                (company-ensure-emulation-alist))))

  (evil-mode 1))

(defun lambda-hs-hide-level-1 ()
  "Just fold level 1 elements."
  (hs-hide-level 1))
(define-key evil-normal-state-map (kbd "zM")
            #'(lambda ()
                (interactive)
                (let ((hs-hide-all-non-comment-function 'lambda-hs-hide-level-1))
                  (evil-close-folds))))

(defun lambda-copy-to-end-of-line ()
  "Copy to end of line, and bind this funciton to Y in normal mode."
  (interactive)
  (evil-yank (point) (point-at-eol)))
;; (define-key evil-normal-state-map (kbd "C-w") 'evil-window-map)

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

(use-package avy
  :ensure t)

;; ;; evil-leader -----------------------------------------------------------------
(use-package evil-leader
  :ensure t
  :config
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "c" 'avy-goto-char
    "w" 'avy-goto-word-1
    "l" 'avy-goto-line)
  (evil-leader/set-key-for-mode 'Info-mode "<SPC>" 'Info-scroll-up)
  (global-evil-leader-mode 1))

(use-package evil-nerd-commenter
  :ensure t)

;; evil-exchange ---------------------------------------------------------------
;; powerful tool to exchange text
;; gx (evil-exchange)
;; gX (evil-exchange-cancel)
;; evil-exchange can be used with ace-jump, it's perfect
(use-package evil-exchange
  :ensure t
  :config
  (evil-exchange-install))

;; evil-matchit ----------------------------------------------------------------
;; Jump between beginning and ending of structure like parens, html tags etc..
(use-package evil-matchit
  :ensure t
  :config
  (global-evil-matchit-mode 1))

;; evil-visualstar -------------------------------------------------------------
(use-package evil-visualstar
  :ensure t
  :config
  (global-evil-visualstar-mode t))
(require 'evil-visualstar)
;; evil-visualstar bugfix in terminal mode
(evil-define-motion evil-visualstar/begin-search-forward (beg end)
  "Search for the visual selection forwards."
  :jump t
  :repeat nil
  (if (use-region-p)
      (evil-visualstar/begin-search (region-beginning) (+ 1 (region-end)) t)))

;; evil-visualstar bugfix in terminal mode
(evil-define-motion evil-visualstar/begin-search-backward (beg end)
  "Search for the visual selection backwards."
  :jump t
  :repeat nil
  (if (use-region-p)
      (evil-visualstar/begin-search (region-beginning) (+ 1 (region-end)) nil)))


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
(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . (lambda () (evil-org-mode)))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package ace-window
  :ensure t
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :bind (("C-x o" . ace-window)
         :map evil-window-map
         ("w" . ace-window)
         ("C-w" . ace-window)))

(use-package evil-commentary
  :ensure t
  :diminish evil-commentary-mode
  :config
  (evil-commentary-mode))

(use-package evil-numbers
  :ensure t
  :bind (:map evil-normal-state-map
              ("C-c +" . evil-numbers/inc-at-pt)
              ("C-c +" . evil-numbers/inc-at-pt)
              ("C-c -" . evil-numbers/dec-at-pt)
              ("C-c -" . evil-numbers/dec-at-pt)))

;; anzu for evil ---------------------------------------------------------------
;; support for * or # search command
;; anzu.el is an Emacs port of anzu.vim. anzu.el provides a minor mode which
;; displays current match and total matches information in the mode-line in
;; various search modes.
(use-package evil-anzu
  :ensure t)

;; define text objects
(defmacro spacemacs|define-text-object (key name start end)
  "Define a text object and a surround pair.
KEY is key of the text object, NAME is the name of text object,
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
KEY is key of the text object, NAME is the name of text object,
START-REGEXP and END-REGEXP are the boundaries of the text
object."
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

;; (use-package evil-cleverparens
;;   :ensure t
;;   :config
;;   (require 'evil-cleverparens-text-objects)
;;   (add-hook 'smartparens-enabled-hook #'evil-cleverparens-mode))

;; work with scroll-all-mode
(with-eval-after-load 'scroll-all
  (defun scroll-all-check-to-scroll ()
    "Check `this-command' to see if a scroll is to be done."
    (cond 
     ((eq this-command 'evil-scroll-line-down)
      (let ((scroll-preserve-screen-position nil))
        (scroll-all-function-all 'scroll-up 1)))
     ((eq this-command 'evil-scroll-line-up)
      (let ((scroll-preserve-screen-position nil))
        (scroll-all-function-all 'scroll-down 1)))

     ((eq this-command 'next-line)
      (call-interactively 'scroll-all-scroll-down-all))
     ((eq this-command 'previous-line)
      (call-interactively 'scroll-all-scroll-up-all))
     ((memq this-command '(scroll-up scroll-up-command))
      (call-interactively 'scroll-all-page-down-all))
     ((memq this-command '(scroll-down scroll-down-command))
      (call-interactively 'scroll-all-page-up-all))
     ((eq this-command 'beginning-of-buffer)
      (call-interactively 'scroll-all-beginning-of-buffer-all))
     ((eq this-command 'end-of-buffer)
      (call-interactively 'scroll-all-end-of-buffer-all))))

  (defun mwheel-scroll-all-function-all (func &optional arg)
    (if (and scroll-all-mode arg)
        (save-selected-window
          (walk-windows
           (lambda (win)
             (select-window win)
             (condition-case nil
                 (funcall func arg)
               (error nil)))))
      (funcall func arg)))

  (defun mwheel-scroll-all-scroll-up-all (&optional arg)
    (mwheel-scroll-all-function-all 'scroll-up arg))

  (defun mwheel-scroll-all-scroll-down-all (&optional arg)
    (mwheel-scroll-all-function-all 'scroll-down arg))

  (setq mwheel-scroll-up-function 'mwheel-scroll-all-scroll-up-all)
  (setq mwheel-scroll-down-function 'mwheel-scroll-all-scroll-down-all)
  )

(provide 'lambda-evil)

;;; lambda-evil.el ends here
