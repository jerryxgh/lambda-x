;;; lambda-sql.el --- for sql editing -*- lexical-binding: t -*-

;; This file is not part of GNU Emacs.

;;; Commentary:

;; for sql editing

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'lambda-sql)

;;; Change Log:

;; Version $(3) 2022-10-29 GuanghuiXu
;;   - Initial release

;;; Code:

(require 'lambda-core)
(require 'lambda-company)

;; sqlup-mode
(use-package sqlup-mode
  :ensure t
  :delight sqlup-mode
  :config
  (setq sqlup-blacklist '("name" "key"))
  (setq sqlup-blacklist nil)
  (add-hook 'sql-mode-hook 'sqlup-mode))

;; sql-indent
(use-package sql-indent
  :ensure t
  :delight sqlind-minor-mode
  :config
  (add-hook 'sql-mode-hook 'sqlind-minor-mode))

(use-package sqlformat
  :ensure t
  :delight sqlformat-on-save-mode
  :config
  (setq sqlformat-command 'sql-formatter)
  (setq sqlformat-args (cons (concat "-c" (concat lambda-package-direcotry "misc/sql-formatter.json")) '()))
  (define-key sql-mode-map (kbd "C-c C-f") 'sqlformat)
  (add-hook 'sql-mode-hook 'sqlformat-on-save-mode))

(add-hook 'sql-mode-hook
          (lambda ()
            (setq company-backends
                  '((lambda-company-yasnippet lambda-company-dabbrev-code lambda-company-dabbrev lambda-company-keywords)))))

(provide 'lambda-sql)

;;; lambda-sql.el ends here
