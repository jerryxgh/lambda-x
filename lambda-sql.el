;;; lambda-sql.el --- for sql editing -*- lexical-binding: t -*-

;; Copyright (C) 2022 Guanghui Xu
;;
;; Author: Guanghui Xu gh_xu@qq.com
;; Maintainer: Guanghui Xu gh_xu@qq.com
;; Created: 2022-10-29
;; Version: 0.0.1
;; Keywords: sql
;; Homepage: not distributed yet
;; Package-Version: 0.0.1
;; Package-Requires: sql-mode
;;

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

;; sqlup-mode
(use-package sqlup-mode
  :ensure t
  :config
  (setq sqlup-blacklist '("name", "key"))
  (add-hook 'sql-mode-hook 'sqlup-mode))

;; sql-indent
(use-package sql-indent
  :ensure t
  :config
  (add-hook 'sql-mode-hook 'sqlind-minor-mode)
  )

(provide 'lambda-sql)

;;; lambda-sql.el ends here
