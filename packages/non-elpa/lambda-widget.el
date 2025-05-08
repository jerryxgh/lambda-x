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
                       ((= char ?\') "\\\'")
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
                             ((= char ?\') "'")
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
