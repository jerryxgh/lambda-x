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
;; Package-Requires: ((emacs "28.1"))
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

(require 'project)

(defun lambda-widget-copy-current-file-path-or-directory ()
  "Copy the current file's absolute path or the current directory."
  (interactive)
  (lambda-widget--copy-current-file-path-or-directory))

(defun lambda-widget-copy-current-file-path-or-directory-relative ()
  "Copy the current path relative to the project root, if available."
  (interactive)
  (lambda-widget--copy-current-file-path-or-directory t))

(defun lambda-widget--copy-current-file-path-or-directory (&optional relative)
  "Copy the current path, relative to the project root when RELATIVE is non-nil."
  (let* ((path (or buffer-file-name
                   (when (derived-mode-p 'dired-mode)
                     (dired-current-directory))
                   default-directory))
         (project (and relative (project-current)))
         (copy-path (if project
                        (file-relative-name
                         (file-truename path)
                         (file-truename (project-root project)))
                      path))
         (select-enable-clipboard t))
    (kill-new copy-path)
    (message "Copied[%s]: %s" (if project "relative" "absolute") copy-path)))

(defun lambda-widget-escape-string (input)
  "Escape newline, carriage return, tab, single quote and quote in INPUT."
  (let ((result ""))
    (dotimes (i (length input))
      (let ((char (aref input i)))
        (setq result
              (concat result
                      (cond
                       ((= char ?\n) "\\n")
                       ((= char ?\r) "\\r")
                       ((= char ?\t) "\\t")
                       ((= char ?\") "\\\"")
                       ;; ((= char ?\') "\\\'")
                       ((= char ?\\) "\\\\")
                       (t (string char)))))))
    result))

(defun lambda-widget-unescape-string (input)
  "Unescape newline, carriage return, tab, single quote and quote in INPUT."
  (let ((result "")
        (escaped nil))
    (dotimes (i (length input))
      (let ((char (aref input i)))
        (if escaped
            (progn
              (setq result
                    (concat result
                            (cond
                             ((= char ?n) "\n")
                             ((= char ?r) "\r")
                             ((= char ?t) "\t")
                             ((= char ?\") "\"")
                             ;; ((= char ?\') "'")
                             ((= char ?\\) "\\")
                             (t (concat "\\" (string char))))))
              (setq escaped nil))
          (if (= char ?\\)
              (setq escaped t)
            (setq result
                  (concat result (string char)))))))
    (if escaped
        (setq result (concat result "\\")))
    result))

(defun lambda-widget-escape-region ()
  "Escape newline, carriage return, tab, single quote and quote in region."
  (interactive)
  (let* ((region-active (use-region-p))
         (start (if region-active (region-beginning) (point-min)))
         (end (if region-active (region-end) (point-max)))
         (text (buffer-substring start end))
         (escaped-text (lambda-widget-escape-string text)))
    (delete-region start end)
    (insert escaped-text)))

(defun lambda-widget-unescape-region ()
  "Unescape newline, carriage return, tab, single quote and quote in region."
  (interactive)
  (let* ((region-active (use-region-p))
         (start (if region-active (region-beginning) (point-min)))
         (end (if region-active (region-end) (point-max)))
         (text (buffer-substring start end))
         (escaped-text (lambda-widget-unescape-string text)))
    (delete-region start end)
    (insert escaped-text)))

(provide 'lambda-widget)

;;; lambda-widget.el ends here
