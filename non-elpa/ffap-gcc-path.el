;;; ffap-gcc-path.el --- get gcc's include path for ffap-c-path

;; Copyright 2007, 2008, 2009, 2011, 2012, 2013, 2014 Kevin Ryde

;; Author: Kevin Ryde <user42_kevin@yahoo.com.au>
;; Version: 8
;; Keywords: files, ffap, C
;; URL: http://user42.tuxfamily.org/ffap-gcc-path/index.html
;; EmacsWiki: FindFileAtPoint

;; ffap-gcc-path.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 3, or (at your option) any later
;; version.
;;
;; ffap-gcc-path.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You can get a copy of the GNU General Public License online at
;; <http://www.gnu.org/licenses/>.


;;; Commentary:

;; The default `ffap-c-path' is "/usr/include" and "/usr/local/include",
;; but gcc has a separate directory for bits it provides, like float.h.
;; This spot of code extracts gcc's path by parsing the output of "gcc -v".
;;
;; The same could be done for `PC-include-file-path' perhaps, but there's
;; nothing for that yet.

;;; Install:

;; Put ffap-gcc-path.el in one of your `load-path' directories and in your
;; .emacs add
;;
;;     (eval-after-load "ffap" '(require 'ffap-gcc-path))
;;

;;; History:
;; 
;; Version 1 - the first version
;; Version 2 - don't side-effect process-environment
;; Version 3 - namespace clean on xemacs (by avoiding what it doesn't have)
;; Version 4 - cope with non-existent `default-directory'
;; Version 5 - no autoload cookie, as it's a user preference really
;;           - fix for non-existant ffap-gcc-program executable
;; Version 6 - use pipe rather than pty for subprocess
;; Version 7 - use `null-device'
;; Version 8 - comments on what happens if no gcc

;;; Code:

(require 'ffap)

(defvar ffap-gcc-program "gcc"
  "The name of the gcc program for `ffap-gcc-path-setup'.")

(defun ffap-gcc-path-setup ()
  "Set `ffap-c-path' to the include path used by gcc.
The gcc program is taken from `ffap-gcc-program'.  You can change
that and re-run `ffap-gcc-path-setup' if cross-compiling or using
a particular gcc version.

If the `ffap-gcc-program' doesn't exist then a message is given
and `ffap-c-path' is unchanged.  This means that loading
ffap-gcc-path.el doesn't cause an error if you don't have gcc.

The ffap-gcc-path.el home page is
URL `http://user42.tuxfamily.org/ffap-gcc-path/index.html'"

  (with-temp-buffer
    (setq default-directory "/") ;; in case inherit non-existent
    (let ((ret (let ((process-environment (copy-sequence process-environment))
                     (process-connection-type nil)) ;; pipe
                 ;; The messages "search starts here" etc are probably
                 ;; translated, avoid that.
                 (setenv "LANGUAGES" nil)
                 (setenv "LANG" nil)
                 (setenv "LC_ALL" nil)
                 (setenv "LC_MESSAGES" nil)
                 (condition-case err
                     (call-process ffap-gcc-program
                                   nil  ;; input
                                   t    ;; output to current buffer
                                   nil  ;; display
                                   "-v"
                                   "-E"
                                   "--language=c"
                                   (if (boundp 'null-device)
                                       null-device ;; new in Emacs 20.3
                                     "/dev/null"))
                   ;; `ret' gets message string on error
                   (error (error-message-string err))))))
      (if (not (equal 0 ret))
          ;; want to tolerate no gcc available at all, so just `message' here
          (message "Error running %s: %s" ffap-gcc-program ret)

        ;; note the regexp subexpr here avoids matching last \n because
        ;; emacs 22 made an incompatible change to split-string, a trailing
        ;; separator there results in an empty string at the end
        (goto-char (point-min))
        (or (re-search-forward "#include <\\.\\.\\.> search starts here:
\\(.*\\(\n.*\\)*\\)\nEnd of search list." nil t)
            (error "%s search path output unrecognised" ffap-gcc-program))

        (narrow-to-region (match-beginning 1) (match-end 1))
        (goto-char (point-min))
        (while (re-search-forward "^ +" nil t) ;; lose spaces at start of each
          (replace-match "" t t))
        (setq ffap-c-path (split-string (buffer-string) "\n"))))))

;; do the setup now
(ffap-gcc-path-setup)

;; LocalWords: usr gcc gcc's el

(provide 'ffap-gcc-path)

;;; ffap-gcc-path.el ends here
