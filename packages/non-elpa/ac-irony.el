;;; ac-irony.el --- Auto-complete support for irony-mode

;; Copyright (C) 2011-2014  Guillaume Papin

;; Author: Guillaume Papin <guillaume.papin@epitech.eu>
;; Version: 0.1.0
;; URL: https://github.com/Sarcasm/ac-irony/
;; Package-Requires: ((auto-complete "1.4") (irony"0.1"))
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
(require 'subr-x)


;;
;; AC Irony
;;

;;;###autoload
(defvar ac-source-irony
  '((candidates . ac-irony-candidates)
    (prefix     . ac-irony-prefix)
    (document   . ac-irony-document)
    (cache)
    (summary    . "candidates from irony")
    (requires   . 0)
    (symbol     . "i")
    ))

(defun ac-irony-complete ()
  "Do complete use `ac-source-irony'."
  (interactive)
  (auto-complete '(ac-source-irony)))

(defun ac-irony-complete-async ()
  "Send complete msg async."
  (interactive)
  (irony-completion-candidates-async 'ac-irony-complete))

(defun ac-irony-prefix ()
  "Prefix for ac-irony completing."

  (let (cc-member-prefix)
    (save-excursion
      (setq cc-member-prefix (ac-prefix-cc-member)))

    (if cc-member-prefix
        (if (irony-completion-candidates-available-p)
            (irony-completion-beginning-of-symbol)
          (ac-irony-complete-async)
          nil))))

(defun ac-irony-candidates ()
  "Get candidates from irony."
  (mapcar #'(lambda (candidate)
              (car candidate))
          (irony-completion-candidates)))

(defun ac-irony-document (symbol)
  "Look up documentation for SYMBOL in the completion candidates list."
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
