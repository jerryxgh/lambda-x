;;; ac-irony.el --- Auto-complete support for irony-mode

;; Copyright (C) 2011-2014  Guillaume Papin

;; Author: Guillaume Papin <guillaume.papin@epitech.eu>
;; Version: 0.1.0
;; URL: https://github.com/Sarcasm/ac-irony/
;; Package-Requires: ((auto-complete "1.4") (irony-mode "0.1"))
;; Keywords: c, convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Asynchronous completion is triggered by `ac-complete-irony-async'. When the
;; completion results are available, the auto-complete popup shows up. Bind this
;; to the key of your choice.
;;
;; Usage:
;;   (add-to-list 'load-path "/path/to/ac-irony")
;;   (require 'ac-irony)
;;
;;   (defun my-ac-irony-setup ()
;;     (add-to-list 'ac-sources 'ac-source-irony)
;;     (auto-complete-mode 1)
;;     (define-key irony-mode-map (kbd "M-RET") 'ac-complete-irony-async))
;;
;;   (add-hook 'irony-mode-hook 'my-ac-irony-setup)

;;; Code:

(require 'irony-completion)

(require 'auto-complete)


;;
;; AC Irony
;;

;;;###autoload
(defvar ac-source-irony
  '((candidates . ac-irony-candidates)
    (prefix     . ac-irony-prefix)
    (document   . ac-irony-document)
    (cache)
    (summary    . "candidate from irony")
    (requires   . 0)
    (symbol     . "i")
    ))

(defun ac-irony-prefix ()
  ;; do not return a valid prefix until we have the candidates
  ;; (when (irony-completion-candidates-available-p)
  ;;   (irony-completion-beginning-of-symbol))
  (irony-completion-beginning-of-symbol))

(defun ac-irony-candidates ()
  (mapcar #'(lambda (candidate)
              (car candidate))
          (irony-completion-candidates)))

(defun ac-irony-document (symbol)
  "Looks up the documentation string for the given SYMBOL in the
completion candidates list."
  (catch 'break
    (mapc #'(lambda (candidate)
              (if (string= symbol (car candidate))
                  (throw 'break (concat (nth 4 candidate)
                                        " : "
                                        (nth 2 candidate)
                                        "\n\n"
                                        (let ((doc (nth 3 candidate)))
                                          (if (string-empty-p doc)
                                              "Not documented."
                                            doc))))))
          (irony-completion-candidates))))

(provide 'ac-irony)
;;; ac-irony.el ends here
