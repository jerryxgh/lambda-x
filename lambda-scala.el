;;; lambda-scala.el --- Scala

;;; Commentary:

;;; Code:

(require 'lambda-core)

;; scala-mode2 -----------------------------------------------------------------
(lambda-package-ensure-install 'scala-mode2)

;; ensime ----------------------------------------------------------------------
(lambda-package-ensure-install 'ensime)
;; This step causes the ensime-mode to be started whenever
;; scala-mode is started for a buffer. You may have to customize this step
;; if you're not using the standard scala mode.
(add-hook 'scala-mode-hook #'(lambda ()
                               (hs-minor-mode 1)))

(setq ensime-completion-style 'auto-complete)
(when (featurep 'evil)
  (evil-define-key 'normal ensime-mode-map (kbd "M-.") #'ensime-edit-definition)
  (evil-define-key 'insert ensime-mode-map (kbd "M-.") #'ensime-edit-definition))

;; sbt-mode --------------------------------------------------------------------
(lambda-package-ensure-install 'sbt-mode)
(setq sbt:program-name "/home/xgh/local/sbt/bin/sbt")

(defadvice sbt:run-sbt (after kill-buffer-when-sbt-quit activate)
  "Kill sbt buffer when quit sbt."
  (let ((buffer ad-return-value))
    (if (bufferp buffer)
        (kill-buffer-when-shell-command-exit buffer))))

(add-hook 'sbt-mode-hook
          '(lambda ()
             ;; compilation-skip-threshold tells the compilation minor-mode
             ;; which type of compiler output can be skipped. 1 = skip info
             ;; 2 = skip info and warnings.
             (setq compilation-skip-threshold 1)

             ;; Bind C-a to 'comint-bol when in sbt-mode. This will move the
             ;; cursor to just after prompt.
             (local-set-key (kbd "C-a") 'comint-bol)

             ;; Bind M-RET to 'comint-accumulate. This will allow you to add
             ;; more than one line to scala console prompt before sending it
             ;; for interpretation. It will keep your command history cleaner.
             (local-set-key (kbd "M-RET") 'comint-accumulate)
             ))

(provide 'lambda-scala)

;;; lambda-scala.el ends here
