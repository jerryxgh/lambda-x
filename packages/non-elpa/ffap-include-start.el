;;; ffap-include-start.el --- recognise C #include when at start of line

;; Copyright 2007, 2009, 2010, 2011, 2013 Kevin Ryde

;; Author: Kevin Ryde <user42@zip.com.au>
;; Version: 10
;; Keywords: files, ffap, C, make, gtk
;; URL: http://user42.tuxfamily.org/ffap-include-start/index.html
;; EmacsWiki: FindFileAtPoint

;; ffap-include-start.el is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; ffap-include-start.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You can get a copy of the GNU General Public License online at
;; <http://www.gnu.org/licenses/>.


;;; Commentary:

;; M-x ffap normally only recognises an include like
;;
;;     #include <foo.h>
;;
;; when point is on the filename part.  This spot of code lets it work when
;; point in the "#include" part.  The following forms are supported,
;;
;;     #include <foo.h>       C language
;;     #include "foo.h"       C language
;;     @include "foo.awk"     GNU Awk
;;     include foo.make       GNU Make
;;     include "foo.rc"       Gtk RC file
;;
;; You can always move point to the filename and M-x ffap from there, but
;; it's handy to have it work from the start of the line too, especially
;; when just browsing rather than editing.
;;
;; GNU Make can do a multiple-file include.  The first filename is offered
;; when point is on the include.  Move point to the second name to get that.
;;
;;     include foo.make bar.make
;;
;; This code works with ffap-makefile-vars.el.  If you load that package
;; then a GNU Make include can have variables to expand,
;;
;;     include $(HOME)/mystuff/foo.make

;;; Emacsen:

;; Designed for Emacs 20 up and XEmacs 21 up.

;;; Install:

;; Put ffap-include-start.el in one of your `load-path' directories and the
;; following in your .emacs
;;
;;     (eval-after-load "ffap" '(require 'ffap-include-start))

;;; History:
;; 
;; Version 1 - the first version
;; Version 2 - GPLv3
;; Version 3 - set region for ffap-highlight
;; Version 4 - allow missing closing quote, don't extend across newline
;; Version 5 - set ffap-string-at-point variable
;; Version 6 - recognise gnu make include too
;; Version 7 - recognise gtk rc include too
;; Version 8 - undo defadvice on unload-feature
;; Version 9 - speedup for big buffers
;; Version 10 - add gnu awk @include


;;; Code:

;;;###autoload (eval-after-load "ffap" '(require 'ffap-include-start))

;; Explicit dependency on advice.el since
;; `ffap-include-start-unload-function' needs `ad-find-advice' macro when
;; running not byte compiled, and that macro is not autoloaded.
(require 'advice)

(defadvice ffap-string-at-point (around ffap-include-start activate)
  "Recognise various \"include /my/file/name.x\" with point on the \"include\"."

  (require 'thingatpt)
  (if (save-restriction
        ;; Narrow to the current line as a speedup for big buffers.  This
        ;; limits the amount of searching forward and back that
        ;; `thing-at-point-looking-at' does when it works-around the way
        ;; `re-search-backward' doesn't match across point.
        ;;
        (narrow-to-region (line-beginning-position) (line-end-position))

        (or
         ;;  GNU Awk    @include "foo.awk"
         ;; Normally at the start of a line, but allow elsewhere in case
         ;; commented out.  Spaces and tabs work after the @.  Dunno if
         ;; that's a documented gawk feature but allow it here.
         (thing-at-point-looking-at "@[ \t]*include[ \t]+\"\\([^ \t\r\n\"]+\\)\\(\"\\|$\\)")

         ;;  Gtk RC     include "foo.rc"
         ;; Normally at the start of a line, but allow it elsewhere in case
         ;; commented out.  Commented out with "#" will in fact look like a
         ;; C #include.
         (thing-at-point-looking-at "include[ \t]+\"\\([^ \t\r\n\"]+\\)\\(\"\\|$\\)")

         ;;  GNU Make     include foo.make
         ;; This is only at the start of a line because an unquoted filename
         ;; would be too ambiguous in the middle of a line.
         (thing-at-point-looking-at "^include[ \t]+\\([^ \t\r\n]+\\)")

         ;; C/C++        #include "foo.h"
         ;;              #include <foo.h>
         (thing-at-point-looking-at "#[ \t]*include[ \t]+[\"<]\\([^\">\r\n]+\\)\\([\">]\\|$\\)")))

      (progn
        (setq ffap-string-at-point-region (list (match-beginning 1)
                                                (match-end 1)))
        (setq ad-return-value
              (setq ffap-string-at-point ;; and return the value
                    (buffer-substring-no-properties (match-beginning 1)
                                                    (match-end 1)))))
    ad-do-it))

(defun ffap-include-start-unload-function ()
  "Remove defadvice from `ffap-string-at-point'.
This is called by `unload-feature'."
  (when (ad-find-advice 'ffap-string-at-point 'around 'ffap-include-start)
    (ad-remove-advice   'ffap-string-at-point 'around 'ffap-include-start)
    (ad-activate        'ffap-string-at-point))
  nil) ;; and do normal unload-feature actions too

(provide 'ffap-include-start)

;;; ffap-include-start.el ends here
