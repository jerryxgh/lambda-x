;;; ffap-I-option.el --- recognise -I/usr/include/foo

;; Copyright 2011 Kevin Ryde

;; Author: Kevin Ryde <user42@zip.com.au>
;; Version: 1
;; Keywords: files
;; URL: http://user42.tuxfamily.org/ffap-I-option/index.html
;; EmacsWiki: FindFileAtPoint

;; ffap-I-option.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 3, or (at your option) any later
;; version.
;;
;; ffap-I-option.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You can get a copy of the GNU General Public License online at
;; <http://www.gnu.org/licenses/>.


;;; Commentary:

;; This spot of code extends M-x ffap to recognise -I command line options
;; like
;;
;;     -I/usr/include/something/
;;
;; See the `ffap-I-option-enable' docstring below for more.

;;; Install:

;; Put ffap-I-option.el in one of your `load-path' directories and the
;; following in your .emacs
;;
;;     (autoload 'ffap-I-option-enable "ffap-I-option" nil t)
;;
;; Then to enable it when ffap loads
;;
;;     (eval-after-load "ffap" '(ffap-I-option-enable))
;;
;; The function is interactive so `M-x ffap-I-option-enable' can be used to
;; try it or if only wanted sometimes, etc.
;;
;; There's an autoload cookie for `ffap-I-option-enable' if you know how to
;; use `update-file-autoloads' and friends, then just `eval-after-load' or
;; `M-x'.

;;; History:
;; 
;; Version 1 - the first version


;;; Code:

(require 'ffap)

;; for `ad-find-advice' macro when running uncompiled
;; (don't unload 'advice before ffap-I-option-unload-function runs)
(require 'advice)

(defadvice ffap-string-at-point (around ffap-I-option disable)
  "Recognise -I/usr/include/foo options as directories."

  ;; Mangle ffap-string-at-point rather than ffap-file-at-point since with
  ;; the former a string of a possibly non-existent directory will be pruned
  ;; back.  Eg. plain ffap on /tmp/no/such offers /tmp, and likewise
  ;; -I/tmp/no/such offers /tmp.
  ;;
  ;; Narrow to the current line as a speedup for big buffers.  It limits the
  ;; amount of searching forward and back thing-at-point-looking-at will do
  ;; in its workaround of the way re-search-backward doesn't match across
  ;; point.
  ;;
  (require 'thingatpt)
  (if (save-restriction
        (narrow-to-region (line-beginning-position) (line-end-position))
        (and (let ((case-fold-search nil)) ;; capital -I only
               ;; non-word or ' or " before -I, then filename/dirname of
               ;; non-whitespace and not ' or "
               (thing-at-point-looking-at
                "\\(\\`\\|\\(['\"]\\)\\|\\Sw\\)-I['\"]?\\([^ \t\r\n\f'\"]+\\)"))
             (or (match-beginning 2)           ;; ' or " ok
                 (>= (point) (match-end 1))))) ;; word char not ok

      (progn
        (setq ffap-string-at-point-region (list (match-beginning 3)
                                                (match-end 3)))
        (setq ad-return-value
              (setq ffap-string-at-point ;; and return the value
                    (buffer-substring-no-properties (match-beginning 3)
                                                    (match-end 3)))))
    ad-do-it))

;;;###autoload
(defun ffap-I-option-enable ()
  "Extend `ffap' to recognise -I/usr/include/something/.
This is the sort of command line option found in makefiles or
compile logs.

    gcc -I/usr/include/something/ foo.c

`ffap' on its own takes the \"I\" is part of the filename and so
doesn't find the intended directory.  (With a space after the
\"-I\" it's fine though.)

The current code is restricted to capital -I, as this is enough
for various compilers and interpreters and should minimize the
chance of misinterpreting something else.

The `ffap-I-option' home page is
URL `http://user42.tuxfamily.org/ffap-I-option/index.html'"

  (interactive)
  (ad-enable-advice 'ffap-string-at-point 'around 'ffap-I-option)
  (ad-activate      'ffap-string-at-point))

(defun ffap-I-option-unload-function ()
  "Remove defadvice from function `ffap-string-at-point'
This is called by `unload-feature'."
  (when (ad-find-advice 'ffap-string-at-point 'around 'ffap-I-option)
    (ad-remove-advice   'ffap-string-at-point 'around 'ffap-I-option)
    (ad-activate        'ffap-string-at-point))
  nil) ;; and do normal unload-feature actions too

;; LocalWords: usr makefiles docstring

(provide 'ffap-I-option)

;;; ffap-I-option.el ends here
