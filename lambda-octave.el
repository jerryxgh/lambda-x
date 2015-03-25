;;; lambda-octave.el --- octave script

;;; Commentary:

;;; Code:

(require 'lambda-core)

(autoload 'octave-mode "octave-mod" nil t)

(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))

(add-hook 'octave-mode-hook
          #'(lambda ()
              (abbrev-mode 1)
              (auto-fill-mode 1)))

(provide 'lambda-octave)

;;; lambda-octave.el ends here
