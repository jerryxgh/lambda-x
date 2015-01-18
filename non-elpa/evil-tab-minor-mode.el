;;; evil-tab-minor-mode.el --- Tweak <tab> behaviour of evil normal mode

;; Copyright 2012, 2013, 2014 Jerry Xu
;;
;; Author: gh_xu@qq.com
;; Version: $Id: evil-tab-minor-mode.el,v 0.0 2012/02/14 12:27:57 jerry Exp $
;; Keywords:
;; X-URL: not distributed yet

;;; Commentary:

;; Tweak <tab> behaviour in evil normal mode.  Note that <tab> is different from
;; TAB.  But evil set TAB and make <tab> same with TAB, this program try to bind
;; <tab> to yas-expand if possible, else bind it to the major mode of C-i or
;; else to the global-map of C-i.

;; Installtion: put this file into your load-path and the following into your
;;   ~/.emacs:

;; (require 'evil-tab-minor-mode)

;;; Code:

(require 'yasnippet)

(defgroup evil-tab nil
  "Customized tab in evil-mode."
  :group 'convenience)

;;;###autoload
(define-minor-mode evil-tab-minor-mode
  "If evil is enabled and yasnippet is not enabled, in
evil-normal-state, <tab> is bound to evil-jump-forward which is
not wanted, change it by using emulation-mode-map-alists"

  ;; The initial value
  :init-value nil
  ;; The indicator for the mode line
  :lighter ""
  :group 'evil-tab
  :global nil
  :version "1.0 beta"

  ;; Body
  (let ((map (make-sparse-keymap))
        (tab-command (evil-tab-get-TAB-command))
        (is-yasnippet-on (and (featurep 'yasnippet)
                              (not (cond ((functionp yas-dont-activate)
                      (funcall yas-dont-activate))
                     ((consp yas-dont-activate)
                      (some #'funcall yas-dont-activate))
                     (yas-dont-activate))))))
    (if is-yasnippet-on
        (setq yas-fallback-behavior
              '(apply evil-tab-yas-fallback-behavior . nil))
      (define-key map (kbd "<tab>") tab-command)
      (set (make-local-variable 'evil-tab-emulation-alist) (list (cons t map)))
      (add-to-list 'emulation-mode-map-alists
                   'evil-tab-emulation-alist
                   'append))))

;;;###autoload
(defun evil-tab-minor-mode-on ()
  "Turn on evil-tab minor mode."
  (unless (minibufferp)
    (evil-tab-minor-mode 1)))

;;;###autoload
(define-globalized-minor-mode
  global-evil-tab-mode
  evil-tab-minor-mode
  evil-tab-minor-mode-on
  :group 'evil-tab
  :require 'evil-tab-minor-mode)

(defun evil-tab-get-TAB-command ()
  "Get the command <tab> should be bound.
According to `major-mode' that yasnippet is not enabled and the
`global-map'."
  (let ((major-mode-map
         (symbol-value (intern-soft (concat (symbol-name major-mode) "-map")))))
    (or (and major-mode-map
             (lookup-key major-mode-map (kbd "C-i")))
        (lookup-key global-map (kbd "C-i")))))

(defun evil-tab-yas-fallback-behavior ()
  "Return a function run by yasnippet's yas-fallback-behavior.
Normally it is indent function."
  (interactive)
  (let ((tab-command (evil-tab-get-TAB-command))
        (old-point (point)))
    ;;(if (functionp tab-command)
    (if (commandp tab-command)
        (command-execute tab-command))
    (if (and (eq (point) old-point)
             (memq (following-char) '(?\) ?\} ?\] ?\> ?\' ?\")))
        (forward-char))))

(provide 'evil-tab-minor-mode)

;;; evil-tab-minor-mode.el ends here
