;;; lambda-thrift.el --- support for thrift -*- lexical-binding: t -*-

;; Copyright (C) 2021 Guanghui Xu

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'lambda-thrift)

;;; Change Log:

;; Version $(3) 2021-11-03 GuanghuiXu
;;   - Initial release

;;; Code:

(require 'lambda-core)
(require 'cc-mode)
(require 'lambda-evil)
(require 'eglot)

;; (use-package semantic-thrift
;;   :ensure t
;;   :config
;;   ;; enable semantic-mode when open thrift file
;;   (setq thrift-indent-level 4)
;;   (add-hook 'thrift-mode-hook (lambda ()
;;                                 (semantic-mode 1)))
;;   ;; only thrift-mode use semantic, since at present most language use lsp instead of semantic
;;   (add-to-list 'semantic-inhibit-functions (lambda () (not (member major-mode '(thrift-mode)))))

;;   (if (bound-and-true-p evil-mode)
;;       ;; support evil-jump
;;       (define-key thrift-mode-map (kbd "M-.") 'evil-goto-definition)
;;     (define-key thrift-mode-map (kbd "M-.") 'semantic-ia-fast-jump))

;;   ;; thrift-mode syntax-table is too weak, it cann't process <> correctly
;;   (setq thrift-mode-syntax-table semantic-thrift-syntax-table))

(defvar semantic-thrift-syntax-table
  (let ((table (make-syntax-table)))

    ;; --- Whitespace ---
    ;; Already set in standard-syntax-table: space, tab, newline, etc.

    ;; --- Word constituents: letters and digits ---
    ;; Standard table already marks a-z, A-Z as 'w', and 0-9 as 'w' (in most locales)
    ;; But to be explicit and safe:
    (let ((i ?a))
      (while (<= i ?z)
        (modify-syntax-entry i "w" table)
        (setq i (1+ i))))
    (let ((i ?A))
      (while (<= i ?Z)
        (modify-syntax-entry i "w" table)
        (setq i (1+ i))))
    (let ((i ?0))
      (while (<= i ?9)
        (modify-syntax-entry i "w" table)
        (setq i (1+ i))))

    ;; Underscore is part of identifiers (e.g., my_struct)
    (modify-syntax-entry ?_ "w" table)

    ;; Minus/hyphen is NOT part of identifiers in Thrift → treat as punctuation
    (modify-syntax-entry ?- "." table)

    ;; Dollar sign is not used in Thrift identifiers
    (modify-syntax-entry ?$ "." table)

    ;; --- Parentheses and delimiters ---
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)

    ;; Commas, semicolons, colons are punctuation
    (modify-syntax-entry ?, "." table)
    (modify-syntax-entry ?\; "." table)
    (modify-syntax-entry ?: "." table)

    ;; Operators like <, >, =, !, etc. are punctuation
    (dolist (c '(?< ?> ?= ?! ?& ?| ?~ ?^ ?% ?+ ?* ?/ ?\\))
      (modify-syntax-entry c "." table))

    ;; --- Strings: only double quotes "..." ---
    (modify-syntax-entry ?\" "\"" table)
    ;; Single quotes are NOT string delimiters in Thrift → treat as punctuation
    (modify-syntax-entry ?\' "." table)

    ;; --- Comments ---
    ;; // style: / is punctuation, but with special comment flags
    (modify-syntax-entry ?/ ". 124b" table)   ; can start // or /*, and help end */
    (modify-syntax-entry ?* ". 23" table)     ; for /* ... */

    ;; # style single-line comment
    (modify-syntax-entry ?# "< b" table)      ; starts comment until newline
    (modify-syntax-entry ?\n "> b" table)     ; newline ends # comment
    (modify-syntax-entry ?\r "> b" table)     ; also handle \r (for \r\n line endings)

    table)
  "Syntax table for Thrift (.thrift) files.")

;; use thrift-ls https://github.com/joyme123/thrift-ls?lang=zh-CN&open_in_browser=true
;; go install github.com/joyme123/thrift-ls@latest
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(thrift-mode . ("thrift-ls"))))

 (use-package semantic-thrift
   :ensure t
   :config
   (add-hook 'thrift-mode-hook #'eglot-ensure)
   (define-key thrift-mode-map (kbd "M-.") #'xref-find-definitions)
   (setq thrift-mode-syntax-table semantic-thrift-syntax-table))

(provide 'lambda-thrift)

;;; lambda-thrift.el ends here
