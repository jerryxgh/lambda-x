;;; lambda-python.el --- Python

;;; Commentary:

;;; Code:

(require 'lambda-core)

;; elpy ------------------------------------------------------------------------
(lambda-package-ensure-install 'elpy)
(setq elpy-rpc-backend "jedi")

(require 'elpy)
(defun lambda-elpy-initialize-variables ()
"This function is used to replace `elpy-initialize-variables'.

Because `elpy-initialize-variables' set something on auto-complete and
yasnippet, which will affect other modes.  Note: this function is almost same as
elpy-initialize-variables, except some lines in `elpy-initialize-variables' are
deleted."
  ;; Local variables in `python-mode'. This is not removed when Elpy
  ;; is disabled, which can cause some confusion.
  (add-hook 'python-mode-hook 'elpy-initialize-local-variables)

  ;; Flymake support using flake8, including warning faces.
  (when (and (executable-find "flake8")
             (not (executable-find python-check-command)))
    (setq python-check-command "flake8"))

  ;; `flymake-no-changes-timeout': The original value of 0.5 is too
  ;; short for Python code, as that will result in the current line to
  ;; be highlighted most of the time, and that's annoying. This value
  ;; might be on the long side, but at least it does not, in general,
  ;; interfere with normal interaction.
  (setq flymake-no-changes-timeout 60)

  ;; `flymake-start-syntax-check-on-newline': This should be nil for
  ;; Python, as;; most lines with a colon at the end will mean the next
  ;; line is always highlighted as error, which is not helpful and
  ;; mostly annoying.
  (setq flymake-start-syntax-check-on-newline nil)

  ;; Elpy provide some YASnippet snippets. Add them.

  ;; yas-snippet-dirs can be a string for a single directory. Make
  ;; sure it's a list in that case so we can add our own entry.
  (when (not (listp yas-snippet-dirs))
    (setq yas-snippet-dirs (list yas-snippet-dirs)))
  (add-to-list 'yas-snippet-dirs
               (concat (file-name-directory (locate-library "elpy"))
                       "snippets/")
               t)

  ;; Now load yasnippets.
  (yas-reload-all))
(defalias 'elpy-initialize-variables 'lambda-elpy-initialize-variables)
(elpy-enable)

(elpy-clean-modeline)

(when (executable-find "ipython")
  (elpy-use-ipython))

(setq elpy-default-minor-modes
	  (remove 'flymake-mode
			  elpy-default-minor-modes))


(provide 'lambda-python)

;;; lambda-python.el ends here
