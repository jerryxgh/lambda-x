;;; lambda-ecb.el --- JavaScript

;;; Commentary:

;;; Code:

(require 'lambda-core)

;; ecb - Emacs code brower -----------------------------------------------------
(lambda-package-ensure-install 'ecb)

(require 'ecb)

(custom-set-variables '(ecb-options-version "2.40"))
(defconst initial-frame-width (frame-width)
  "The width of frame will be changed ,remember the init value.")
(setq ecb-compile-window-height 6
      ecb-compile-window-width 'edit-window
      ecb-compile-window-temporally-enlarge 'both
      ecb-create-layout-file "~/.emacs.d/auto-save-list/.ecb-user-layouts.el"
      ecb-windows-width 30
      ecb-fix-window-size 'width
      ecb-layout-name "jerry"
      ecb-history-make-buckets 'mode
      ecb-kill-buffer-clears-history 'auto
      ecb-tip-of-the-day nil
      ecb-tip-of-the-day-file "~/.emacs/auto-save-list/.ecb-tip-of-day.el"
      ecb-primary-secondary-mouse-buttons 'mouse-1--mouse-2
      semantic-decoration-styles (list '("semantic-decoration-on-includes" . t)
                                       '("semantic-tag-boundary" . t))
      ;; ecb-create-layout-frame-height 40
      ;; ecb-create-layout-frame-width 110
      ecb-auto-activate t
      )

(add-to-list 'ecb-compilation-buffer-names '("*slime-repl sbcl*"))
;;(add-to-list 'ecb-source-path  '("~/Git Repositories/Workspaces" "/root"))

(add-hook 'ecb-show-ecb-windows-before-hook
          'ecb-enlarge-frame-width-before-show)
(add-hook 'ecb-hide-ecb-windows-before-hook
          'ecb-shrink-frame-width-before-hide)
(add-hook 'ecb-deactivate-hook
          'ecb-shrink-frame-width-before-hide)
(add-hook 'ecb-activate-before-layout-draw-hook
          'ecb-enlarge-frame-width-before-activate)

(defun frame-horizontal-maximized-p ()
  "Test current frame wheather be maxmized by test the frame width and height \
equal to the screen resolution"
  (interactive)
  (equal (frame-pixel-width) (display-pixel-width)))

(defun ecb-enlarge-frame-width-before-show ()
  "Enlarge frame width before ecb show layout."
  (if (and (ecb-windows-all-hidden)
           (<= (+ (frame-pixel-width) (* (frame-char-width)
                                         (+ ecb-windows-width 2)))
               (display-pixel-width)))
      (set-frame-width (selected-frame)
					   (+ (frame-width) (+
										 ecb-windows-width 2)))))

(defun ecb-shrink-frame-width-before-hide ()
  "Shrink frame width before ecb hide layout."
  (if (and (not (ecb-windows-all-hidden))
           
           (not (eq (frame-pixel-width)
                    (display-pixel-width))))
      (if (< (- (frame-width) (+ ecb-windows-width 2)) initial-frame-width)
          (set-frame-width (selected-frame) initial-frame-width)
        (set-frame-width (selected-frame)
						 (- (frame-width) (+ ecb-windows-width 2))))))

(defun ecb-enlarge-frame-width-before-activate ()
  "Enlarge frame width when ecb active and need it to."
  (let ((use-last-win-conf
		 (and ecb-last-window-config-before-deactivation
			  (equal ecb-split-edit-window-after-start
					 'before-deactivation)
			  (not (ecb-window-configuration-invalidp
					ecb-last-window-config-before-deactivation)))))
    (unless (or (and use-last-win-conf
                     (eq (nth 5 ecb-last-window-config-before-deactivation)
                         ecb-windows-hidden-all-value))
                (> (+ (frame-pixel-width) (* (frame-char-width)
                                             (+ ecb-windows-width 2)))
                   (display-pixel-width)))
      (set-frame-width (selected-frame) (+ (frame-width) (+ ecb-windows-width 2))))))

;; (defadvice ecb-activate (after ecb-activate-after activate)
;;   "Redraw layout after activation of ecb."
;;   (ecb-redraw-layout))

(define-key ecb-mode-map (kbd "<f9>") 'ecb-toggle-ecb-windows)

;; Layout jerry -----------------------------------------------------

(ecb-layout-define "jerry" right
				   "This function creates the following layout:

   -------------------------------------------------------
   |                                      |              |
   |                                      |  Directories |
   |                                      |              |
   |                                      |              |
   |                                      |              |
   |                                      |              |
   |                                      |--------------|
   |                 Edit                 |              |
   |                                      |  Methods     |
   |                                      |              |
   |                                      |              |
   |                                      |--------------|
   |                                      |              |
   |                                      |  History     |
   |                                      |              |
   -------------------------------------------------------
   |                                                     |
   |                    Compilation                      |
   |                                                     |
   -------------------------------------------------------

If you have not set a compilation-window in `ecb-compile-window-height' then
the layout contains no persistent compilation window and the other windows get a
little more place. This layout works best if it is contained in
`ecb-show-sources-in-directories-buffer'!"
				   (ecb-set-directories-buffer)
				   (ecb-split-ver 0.4)
				   (ecb-set-methods-buffer)
				   (ecb-split-ver 0.5)
				   (ecb-set-history-buffer)
				   (select-window (next-window)))
(add-to-list 'ecb-show-sources-in-directories-buffer "jerry")



(add-hook 'ecb-activate-hook (lambda ()
                               (ecb-hide-ecb-windows-internal 'all)))

(provide 'lambda-ecb)

;;; lambda-ecb.el ends here
