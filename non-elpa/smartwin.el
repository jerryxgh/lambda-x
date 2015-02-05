;;; smartwin.el --- A smart window for temp buffers, shell buffers, etc.

;; Copyright (C) 2015  Jerry Xu

;; Author: Jerry Xu
;; Keywords: convenience
;; Version: 20150124
;; X-Original-Version: 0.1.alpha

;;; Commentary:

;; Smartwin is a window for shell buffers, temp buffers and etc.
;; All temp buffers and shell like buffers share a window, besides that, when
;; point move to the smart window, it is enlarged automaticly, when leave the
;; window, it is shrinked automaticly.
;;
;; To use smartwin, just add the following code into your .emacs:
;;
;;     (require 'smartwin)
;;     (smartwin-mode 1)
;; Then try run shell or show some help, the window will show up.

;;; TODO:

;; 1. enlarge smart window when chosen by mouse
;; 2. add exceptions config
;; 3. process wrong scroll when enlarge smart window
;; 4. window-text-change-functions
;; 5. smart window scroll problem

;;; Code:

(defgroup smartwin nil
  "A smart window for temp buffers, shell like buffers and etc."
  :prefix "smartwin-"
  :version "0.1.alpha"
  :group 'convenience
  :group 'windows)

(defvar smartwin-previous-buffer nil
  "If smart window is hidden, this variable store previous buffer showd.")

(defvar smartwin-max-window-height (- (/ (frame-height) 2) 2)
  "Maximum hight of smart window.
But smart window can be higher if run `delete-other-window' when is is already
  to this height.")

(defvar smartwin-min-window-height 4
  "Maximum hight of smart window.
But smart window can be higher if run `delete-other-window' when is is already
  to this height.")

(defcustom smartwin-buffers
  '(;; all buffers that have no file
    ;; t
    ;; Emacs
    ("*Miniedit Help*" :noselect t)
    help-mode
    (completion-list-mode :noselect t)
    (compilation-mode :noselect t)
    (grep-mode :noselect t)
    (occur-mode :noselect t)
    "*scratch*"
    "*shell*"
    "*eshell*"
    ("*Pp Macroexpand Output*" :noselect t)
    "*Shell Command Output*"
    ;; VC
    "*vc-diff*"
    "*vc-change-log*"
    ;; Undo-Tree
    " *undo-tree*"
    ;; Anything
    ("^\\*anything.*\\*$" :regexp t)
    ;; SLIME
    "*slime-apropos*"
    "*slime-macroexpansion*"
    "*slime-description*"
    ("*slime-compilation*" :noselect t)
    "*slime-xref*"
    sldb-mode
    slime-repl-mode
    slime-connection-list-mode)
  "Configuration of buffers to be shown in smart window.
Available keywords are following:

  regexp: If the value is non-nil, PATTERN will be used as
  regexpto matching buffer.

  noselect: If the value is non-nil, smart window will not be
  selected when it is shown, exception is smart window is already selected.

Examples: With '(\"*Help*\" :noselect t), help buffer will be shown in smart
window and will not be selected."
  :type '(repeat
          (cons :tag "Config"
                (choice :tag "Pattern"
                        (string :tag "Buffer Name")
                        (symbol :tag "Major Mode"))
                (plist :tag "Keywords"
                       :value (:regexp nil) ; BUG? need default value
                       :options
                       ((:regexp (boolean :tag "On/Off"))
                        (:noselect (boolean :tag "On/Off"))))))
  :get (lambda (symbol)
         (mapcar (lambda (element)
                   (if (consp element)
                       element
                     (list element)))
                 (default-value symbol)))
  :group 'smartwin)

(defun smartwin-match-buffer (buffer-or-name)
  "Judge whether to use smartwin to show BUFFER-OR-NAME."
  (let* ((buffer (if (stringp buffer-or-name)
                     (get-buffer buffer-or-name)
                   buffer-or-name))
         (name (buffer-name buffer))
         (mode (buffer-local-value 'major-mode buffer)))
    (catch 'break
      (dolist (config smartwin-buffers)
        (let ((pattern (if (consp config) (car config) config))
              (keywords (if (consp config) (cadr config) nil)))
          (if (cond ((eq pattern t) (not (buffer-file-name buffer)))
                    ((and (stringp pattern) (plist-get keywords :regexp))
                     (string-match pattern name))
                    ((stringp pattern)
                     (string= pattern name))
                    ((symbolp pattern)
                     (eq pattern mode))
                    ((functionp pattern)
                     (funcall pattern buffer))
                    (t (error "Invalid pattern: %s" pattern)))
              (throw 'break config)))))))

(defun smartwin-smart-window-p (window)
  "To judge whether WINDOW is smart window or not."
  (and window (window-parameter window 'smartwinp)))

(defun smartwin-enlarge-window (window)
  "Try to enlarge smart WINDOW, but not too large."
  (let ((height-before (window-height window)))
    (fit-window-to-buffer window
                          smartwin-max-window-height
                          smartwin-min-window-height)
    ;; set smart window start
    (set-window-start window
                      (save-excursion
                        (goto-char (window-start))
                        (forward-line (- height-before (window-height window))))
                      t)))

(defun smartwin-show ()
  "Show smart window."
  (interactive)
  (if (and smartwin-mode
           (not (smartwin-find-smart-win)))
      (let ((window (smartwin-get-smart-win)))
        (if (and smartwin-previous-buffer
                 (buffer-live-p smartwin-previous-buffer))
            (if (not (eq smartwin-previous-buffer (window-buffer window)))
                (set-window-buffer window smartwin-previous-buffer))
          (set-window-buffer window (get-buffer "*scratch*"))))))

(defun smartwin-hide (&optional force)
  "Hide smart window.
If FORCE is non nil, hide smart window forcely."
  (interactive)
  (let ((window (smartwin-get-smart-win)))
    (if window
        (progn
          (setq smartwin-previous-buffer (window-buffer window))
          (if force
              (minimize-window window))
          (delete-window (smartwin-get-smart-win))))))



(defun smartwin-find-smart-win ()
  "Find smart window among all live windows.
If found, return the window, else return nil."
  (catch 'break
    (dolist (window (window-list))
      (if (smartwin-smart-window-p window)
          (throw 'break window)))))

(defun smartwin-get-smart-win ()
  "Create the smart window.
If succeed, created window is returned, else return nil."
  (or (smartwin-find-smart-win)
      (let* ((root-win (frame-root-window))
             (root-height (window-height root-win))
             window)
        (and (not (frame-parameter nil 'unsplittable))
             (setq window
                   (condition-case nil
                       (split-window root-win (- root-height 4) 'below)
                     (error nil)))
             (set-window-parameter window 'smartwinp t))
        window)))

;; switch-to-buffer-other-window
(defun smartwin-get-largest-window (&optional all-frames dedicated not-selected)
  "Like `get-largest-window', but smart window is excluded.
About ALL-FRAMES, DEDICATED and NOT-SELECTED, please see `get-largest-window'"
  (let ((best-size 0)
        best-window size)
    (dolist (window (window-list-1 nil 'nomini all-frames))
      (when (and (not (smartwin-smart-window-p window))
                 (or dedicated (not (window-dedicated-p window)))
		 (or (not not-selected) (not (eq window (selected-window)))))
	(setq size (* (window-total-size window)
		      (window-total-size window t)))
	(when (> size best-size)
	  (setq best-size size)
	  (setq best-window window))))
    best-window))

(defun smartwin-get-lru-window (&optional all-frames dedicated not-selected)
  "Like `get-lru-window', but smart window is excluded.
About ALL-FRAMES, DEDICATED and NOT-SELECTED, please see `get-lru-window'"
  (let (best-window best-time second-best-window second-best-time time)
    (dolist (window (window-list-1 nil 'nomini all-frames))
      (when (and (not (smartwin-smart-window-p window))
                 (or dedicated (not (window-dedicated-p window)))
		 (or (not not-selected) (not (eq window (selected-window)))))
	(setq time (window-use-time window))
	(if (or (eq window (selected-window))
		(not (window-full-width-p window)))
	    (when (or (not second-best-time) (< time second-best-time))
	      (setq second-best-time time)
	      (setq second-best-window window))
	  (when (or (not best-time) (< time best-time))
	    (setq best-time time)
	    (setq best-window window)))))
    (or best-window second-best-window)))

(defun smartwin-get-mru-window (&optional all-frames dedicated not-selected)
  "Like `get-mru-window', but smart window is excluded.
About ALL-FRAMES, DEDICATED and NOT-SELECTED, please see `get-mru-window'"
  (let (best-window best-time time)
    (dolist (window (window-list-1 nil 'nomini all-frames))
      (setq time (window-use-time window))
      (when (and (not (smartwin-smart-window-p window))
                 (or dedicated (not (window-dedicated-p window)))
		 (or (not not-selected) (not (eq window (selected-window))))
		 (or (not best-time) (> time best-time)))
	(setq best-time time)
	(setq best-window window)))
    best-window))

;;; advices

(defadvice quit-window (after smartwin-quit-window)
  "When run `quit-window' in smart window, select other window."
  (let ((window (selected-window)))
    (if (smartwin-smart-window-p window)
        (smartwin-hide))))

(defadvice kill-buffer (around smartwin-kill-buffer)
  "When run `kill-buffer' in smart window, find right buffer to show.
If no buffer can be showed in smart window, hide it."
  (let ((buffer (ad-get-arg 0))
        window)
    (if (not buffer)
        (setq buffer (current-buffer)))
    (setq window (get-buffer-window buffer))
    ad-do-it
    (if (smartwin-smart-window-p window)
        (progn
          (setq buffer
                (catch 'break
                  (dolist (b (window-prev-buffers window))
                    (if (smartwin-match-buffer b)
                        (throw 'break b)))
                  (throw 'break nil)))
          (if buffer
              (set-window-buffer winodw buffer)
            (smartwin-hide t))))))

(defadvice select-window (after smartwin-select-window)
  "Enlarge or shringe smart window when select window."
  (if (not (boundp 'in-smartwin-select-window))
      (let ((in-smartwin-select-window t)
            (window (ad-get-arg 0)))
        (if window
            (let ((smart-win (smartwin-find-smart-win)))
              (if smart-win
                  (if (eq window smart-win)
                      (smartwin-enlarge-window smart-win)
                    (minimize-window smart-win))))))))

(defadvice balance-windows (around smartwin-balance-window-ignore)
  "When `balance-windows', ignore smart window."
  (let* ((window (smartwin-find-smart-win))
         (in-smartwin (eq (selected-window) window)))
    (if window
        (if (not in-smartwin)
            (progn
              (smartwin-hide)
              ad-do-it
              (smartwin-show)
              ))
      ad-do-it)))

(defadvice evil-window-move-far-left (around smartwin-move-far-left-ignore)
  "When `evil-window-move-far-left', ignore smart window."
  (if (not (smartwin-smart-window-p (selected-window)))
      (if (smartwin-find-smart-win)
          (progn
            (smartwin-hide)
            ad-do-it
            (smartwin-show))
        ad-do-it)
    (message "Attempt to move smart window")))

(defadvice evil-window-move-far-right (around smartwin-move-far-right-ignore)
  "When `evil-window-move-far-right', ignore smart window."
  (if (not (smartwin-smart-window-p (selected-window)))
      (if (smartwin-find-smart-win)
          (progn
            (smartwin-hide)
            ad-do-it
            (smartwin-show))
        ad-do-it)
    (message "Attempt to move smart window")))

(defadvice evil-window-move-very-top (around smartwin-move-very-top-ignore)
  "When `evil-window-move-very-top', ignore smart window."
  (if (not (smartwin-smart-window-p (selected-window)))
      (if (smartwin-find-smart-win)
          (progn
            (smartwin-hide)
            ad-do-it
            (smartwin-show))
        ad-do-it)
    (message "Attempt to move smart window")))

(defadvice evil-window-move-very-bottom (around smartwin-move-very-bottom-ignore)
  "When `evil-window-move-very-bottom', ignore smart window."
  (if (not (smartwin-smart-window-p (selected-window)))
      (if (smartwin-find-smart-win)
          (progn
            (smartwin-hide)
            ad-do-it
            (smartwin-show))
        ad-do-it)
    (message "Attempt to move smart window")))

(defadvice evil-window-rotate-upwards (around smartwin-rotate-upwards-ignore)
  "When `evil-window-rotate-upwards', ignore smart window."
  (if (not (smartwin-smart-window-p (selected-window)))
      (if (smartwin-find-smart-win)
          (progn
            (smartwin-hide)
            ad-do-it
            (smartwin-show))
        ad-do-it)))

(defadvice evil-window-rotate-downwards (around smartwin-rotate-downwards-ignore)
  "When `evil-window-rotate-downwards', ignore smart window."
  (if (not (smartwin-smart-window-p (selected-window)))
      (if (smartwin-find-smart-win)
          (progn
            (smartwin-hide)
            ad-do-it
            (smartwin-show))
        ad-do-it)))

(defadvice windmove-do-window-select (around smartwin-ignore-smart-window)
  "When move to smart window, enlarge it, when leave it, shringe it."
  (let ((window-before (selected-window)))
    ad-do-it
    (let ((window-after (selected-window)))
      (if (and (smartwin-smart-window-p window-before)
               (neq window-before window-after))
          (minimize-window window-before)
        (if (and (smartwin-smart-window-p window-after)
                 (neq window-before window-after))
            (smartwin-enlarge-window window-after))))))

(defadvice delete-other-windows (around smartwin-delete-other-windows)
  "If in smart window, just enlarge it instead of `delete-other-windows'."
  (let ((window (selected-window)))
    (if (smartwin-smart-window-p window)
        (smartwin-enlarge-window window)
      (if (or (not (smartwin-find-smart-win)) (eq 2 (length (window-list))))
          ad-do-it
        (smartwin-hide)
        ad-do-it
        (smartwin-show)))))

(defadvice delete-window (around smartwin-delete-window)
  "If the window is last oridnary window(not smart window), do not kill it."
  (let ((window (ad-get-arg 0)))
    (if (not window)
        (setq window (selected-window)))
    (if (smartwin-smart-window-p window)
        (if (< window-min-height (window-height window))
            (minimize-window window)
          (progn
            (setq smartwin-previous-buffer (window-buffer window))
            ad-do-it))
      (if (and (smartwin-find-smart-win) (eq 2 (length (window-list))))
          (progn
            (message "Attempt to delete last oridnary window")
            (setq ad-return-value nil))
        ad-do-it))))

(defadvice get-largest-window (around smartwin-get-largest-window-exclude)
  "Like `get-largest-window', but exclude smart window."
  (let ((all-frames (ad-get-arg 0))
        (dedicated (ad-get-arg 1))
        (not-selected (ad-get-arg 2)))
    (setq ad-return-value
          (smartwin-get-largest-window all-frames dedicated not-selected))))

(defadvice get-lru-window (around smartwin-get-lru-window-exclude)
  "Like `get-lru-window', but exclude smart window."
  (let ((all-frames (ad-get-arg 0))
        (dedicated (ad-get-arg 1))
        (not-selected (ad-get-arg 2)))
    (setq ad-return-value
          (smartwin-get-lru-window all-frames dedicated not-selected))))

(defadvice get-mru-window (around smartwin-get-mru-window-exclude)
  "Like `get-mru-window', but exclude smart window."
  (let ((all-frames (ad-get-arg 0))
        (dedicated (ad-get-arg 1))
        (not-selected (ad-get-arg 2)))
    (setq ad-return-value
          (smartwin-get-mru-window all-frames dedicated not-selected))))

(defadvice split-window (around smartwin-unsplittable)
  "Make smart window unsplittable."
  (let ((window (ad-get-arg 0)))
    (if (smartwin-smart-window-p window)
        (progn (message "Smart window is unsplittable")
               (setq ad-return-value nil))
      ad-do-it)))

(defadvice window-splittable-p (around smartwin-window-splittable-p)
  "Return nil when parameter of `window-splittable-p' is smart window.
This advice will affect `split-window-sensibly' to make it not
split smart window."
  (let ((window (ad-get-arg 0)))
    (if (smartwin-smart-window-p window)
        (setq ad-return-value nil)
      ad-do-it)))

(defadvice switch-to-buffer (around smartwin-switch-to-buffer)
  "When a bffer should be shown in smart window, chage window to smart window."
  (let ((buffer-or-name (ad-get-arg 0)))
    (if (smartwin-match-buffer buffer-or-name)
        (let ((smart-win (smartwin-get-smart-win)))
          (with-selected-window smart-win
            ad-do-it)
          (select-window smart-win))
      (let ((window (selected-window)))
        (if (smartwin-smart-window-p window)
            (switch-to-buffer-other-window buffer-or-name)
          ad-do-it)))))

(defadvice switch-to-buffer-other-window
  (around smartwin-switch-to-buffer-other-window)
  "When a bffer should be show in smart window, chage window to smart window."
  (let ((buffer-or-name (ad-get-arg 0)))
    (if (smartwin-match-buffer buffer-or-name)
        (let ((smart-win (smartwin-get-smart-win)))
          (with-selected-window smart-win
            ad-do-it)
          (select-window smart-win)
          )
      ad-do-it)))

(defadvice display-buffer-pop-up-window
  (around smartwin-display-buffer-pop-up-window)
  "Do not split smart window when run this function."
  (let ((smart-win (smartwin-find-smart-win)))
    (if (not smart-win)
        ad-do-it
      (if (not (window-dedicated-p smart-win))
          (set-window-dedicated-p smart-win t))
      ad-do-it
      (if (window-dedicated-p smart-win)
          (set-window-dedicated-p smart-win nil)))))

;;; Minor Mode

(defun smartwin-display-buffer-condition (buffer-or-name action)
  "Part of pair in `display-buffer-alist'.
BUFFER-OR-NAME is a buffer name, ACTION is an argument of `display-buffer'."
  (and (smartwin-match-buffer buffer-or-name) t))

(defun smartwin-display-buffer-action (buffer-or-name alist)
  "Part of pair in `display-buffer-alist'.
BUFFER-OR-NAME is a buffer to display, ALIST is them same form as ALIST."
  (let ((buffer (window-normalize-buffer-to-switch-to buffer-or-name))
        (smart-win (smartwin-get-smart-win)))
    (with-selected-window smart-win
      (set-window-buffer smart-win buffer)
      ;; (with-no-warnings
      ;;   (if (>= emacs-major-version 24)
      ;;       (switch-to-buffer buffer-or-name nil t)
      ;;     (switch-to-buffer buffer-or-name nil)))
      )
    smart-win))

;;;###autoload
(define-minor-mode smartwin-mode
  "Smartwin is a window for showing shell like buffers, temp buffers and etc."
  :init-value nil
  :global t
  (if (boundp 'display-buffer-alist)
      (let ((pair '(smartwin-display-buffer-condition
                    smartwin-display-buffer-action)))
        (if smartwin-mode
            (progn
              (push pair display-buffer-alist)
              (ad-activate 'switch-to-buffer)
              (ad-activate 'switch-to-buffer-other-window)
              ;;(ad-activate 'windmove-do-window-select)
              ;;(ad-activate 'evil-window-move-very-top)
              ;;(ad-activate 'evil-window-move-far-left)
              ;;(ad-activate 'evil-window-move-far-right)
              ;;(ad-activate 'evil-window-move-very-bottom)
              ;;(ad-activate 'evil-window-rotate-upwards)
              ;;(ad-activate 'evil-window-rotate-downwards)
              ;;(ad-activate 'kill-buffer)
              ;;(ad-activate 'quit-window)
              (ad-activate 'split-window)
              (ad-activate 'display-buffer-pop-up-window)
              (ad-activate 'window-splittable-p)
              (ad-activate 'get-largest-window)
              (ad-activate 'get-lru-window)
              (ad-activate 'get-mru-window)
              (ad-activate 'delete-window)
              (ad-activate 'delete-other-windows)
              (ad-activate 'balance-windows)
              (ad-activate 'select-window)
              (smartwin-show))
          (setq display-buffer-alist (delete pair display-buffer-alist))
          (ad-deactivate 'switch-to-buffer)
          (ad-deactivate 'switch-to-buffer-other-window)
          ;;(ad-deactivate 'windmove-do-window-select)
          ;;(ad-deactivate 'evil-window-move-very-top)
          ;;(ad-deactivate 'evil-window-move-far-left)
          ;;(ad-deactivate 'evil-window-move-far-right)
          ;;(ad-deactivate 'evil-window-move-very-bottom)
          ;;(ad-deactivate 'evil-window-rotate-upwards)
          ;;(ad-deactivate 'evil-window-rotate-downwards)
          ;;(ad-deactivate 'kill-buffer)
          ;;(ad-deactivate 'quit-window)
          (ad-deactivate 'split-window)
          (ad-deactivate 'display-buffer-pop-up-window)
          (ad-deactivate 'window-splittable-p)
          (ad-deactivate 'get-largest-window)
          (ad-deactivate 'get-lru-window)
          (ad-deactivate 'get-mru-window)
          (ad-deactivate 'delete-window)
          (ad-deactivate 'delete-other-windows)
          (ad-deactivate 'balance-windows)
          (ad-deactivate 'select-window)
          (smartwin-hide)
          ))
    (with-no-warnings
      (unless (or (null display-buffer-function)
                  (eq display-buffer-function 'popwin:display-buffer))
        (warn "Overwriting display-buffer-function variable to
        enable/disable popwin-mode"))
      (setq display-buffer-function
            (if popwin-mode 'popwin:display-buffer nil)))))

(provide 'smartwin)
;;; smartwin.el ends here
