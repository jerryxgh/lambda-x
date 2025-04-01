;;; lambda-widget.el --- Little widget for myself -*- lexical-binding: t -*-

;; Copyright (C) 2025 Guanghui Xu
;;
;; Author: Guanghui Xu gh_xu@qq.com
;; Maintainer: Guanghui Xu gh_xu@qq.com
;; Created: 2025-04-01
;; Version: 0.0.1
;; Keywords: convenience
;; Homepage: https://github.com/jerryxgh
;; Package-Version:
;; Package-Requires: ((emacs "24.3"))
;;

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'lambda-widget)

;;; Change Log:

;; Version $(3) 2025-04-01 GuanghuiXu
;;   - Initial release

;;; Code:

(require 'dired-subtree)

(defun lambda-widget-copy-current-file-path-or-directory ()
  "Copy current buffer file'a path if exist, else try `default-directory`."
  (interactive)
  (let ((filename))
    (cond
     ;; In buffers with file name
     ((buffer-file-name)
      (setq filename (buffer-file-name)))
     ((eq major-mode 'dired-mode)
      (set filename (dired-current-directory)))
     ((setq filename default-directory)))
    (kill-new filename)
    (message "'%s' is copied to the clipboard." filename)))

;; (defun lambda-widget-escape-string ()
;;   "Replace \" with \\\" and ewline with \\n in the selected text or the whole buffer."
;;   (interactive)
;;   (let* ((beg (if (use-region-p) (region-beginning) (point-min)))
;;          (end (if (use-region-p) (region-end) (point-max)))
;;          (text (buffer-substring-no-properties beg end))
;;          (processed-text (replace-regexp-in-string "\n" "\\\\n" text)))
;;     (setq processed-text (replace-regexp-in-string "\"" "\\\\\"" processed-text))
;;     (setq processed-text (replace-regexp-in-string "\\" "\\\\" processed-text))
;;     (delete-region beg end)
;;     (insert processed-text)))

;; (defun lambda-widget-unescape-string ()
;;     "Replace \\\" with \" and \\n with newline in the selected text or the whole buffer."
;;   (interactive)
;;   (let* ((beg (if (use-region-p) (region-beginning) (point-min)))
;;          (end (if (use-region-p) (region-end) (point-max)))
;;          (text (buffer-substring-no-properties beg end))
;;          (processed-text (replace-regexp-in-string "\\\\n" "\n" text)))
;;     (setq processed-text (replace-regexp-in-string "\\\\\"" "\"" processed-text))
;;     (setq processed-text (replace-regexp-in-string "\\\\" "\\" processed-text))
;;     (delete-region beg end)
;;     (insert processed-text)))

(defun lambda-widget-escape-text (text)
  "Escape backslashes, double quotes, and newlines in TEXT."
  (replace-regexp-in-string
   "\n" "\\\\n"
   (replace-regexp-in-string
    "\"" "\\\\\""
    (replace-regexp-in-string
     "\\\\" "\\\\\\\\" text))))

(defun lambda-widget-escape-string ()
  "Escape the current buffer or active region by replacing:
\\n -> \\\\\\n (newline)
\" -> \\\\\\\" (double quote)
\\ -> \\\\\\\\ (backslash)"
  (interactive)
  (let* ((region-active (use-region-p))
         (start (if region-active (region-beginning) (point-min)))
         (end (if region-active (region-end) (point-max)))
         (text (buffer-substring start end))
         (escaped-text (lambda-widget-escape-text text)))
    (delete-region start end)
    (insert escaped-text)))

(defun lambda-widget-unescape-text (text)
  "Unescape backslashes, double quotes, and newlines in TEXT."
  (replace-regexp-in-string
   "\\\\n" "\n"
   (replace-regexp-in-string
    "\\\\\"" "\""
    (replace-regexp-in-string
     "\\\\\\\\" "\\\\" text))))

(defun lambda-widget-unescape-string ()
  "Unescape the current buffer or active region by replacing:
\\\\\\n -> \\n (newline)
\\\\\\\" -> \" (double quote)
\\\\\\\\ -> \\ (backslash)"
  (interactive)
  (let* ((region-active (use-region-p))
         (start (if region-active (region-beginning) (point-min)))
         (end (if region-active (region-end) (point-max)))
         (text (buffer-substring start end))
         (escaped-text (lambda-widget-unescape-text text)))
    (delete-region start end)
    (insert escaped-text)))

(provide 'lambda-widget)

;;; lambda-widget.el ends here
