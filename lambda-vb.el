;;; lambda-vb.el --- A testing place -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'lambda-core)

(autoload 'vbnet-mode "vbnet-mode" "Mode for editing VB.NET code." t)
(setq auto-mode-alist (append '(("\\.\\(frm\\|bas\\|cls\\|vb\\|vbs\\)$" .
                                 vbnet-mode)) auto-mode-alist))

(add-hook 'vbnet-mode-hook #'(lambda ()
                               "Hook for VB.NET mode."
                               (turn-on-font-lock)
                               (turn-on-auto-revert-mode)
                               (setq indent-tabs-mode nil)
                               (require 'flymake)
                               (flymake-mode 1)))
(custom-set-faces '(vbnet-funcall-face ((t nil))))

(provide 'lambda-vb)

;;; lambda-vb.el ends here
