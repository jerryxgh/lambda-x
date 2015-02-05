;;; lambda-java.el --- Java

;;; Commentary:

;;; Code:

(require 'lambda-core)
(require 'lambda-cc)
;; eclim plugin: using eclipse feature in emacs --------------------------------
(lambda-package-ensure-install 'emacs-eclim)
(require 'eclim)
(require 'eclimd)
;;(require 'eclim-java)
;;(require 'eclim-completion)
(setq eclimd-wait-for-process nil
      help-at-pt-display-when-idle t
      help-at-pt-timer-delay 0.1)

;; move compile to the first
(setq eclim-maven-lifecycle-phases (delete "compile"
                                           eclim-maven-lifecycle-phases))
(add-to-list 'eclim-maven-lifecycle-phases "compile")

(when (eq system-type 'gnu/linux)
  (add-to-list 'eclim-eclipse-dirs
               "/home/xgh/local/sts-bundle3.6.3/sts-3.6.3.SR1/")
  (setq eclim-executable "/home/xgh/local/sts-bundle3.6.3/sts-3.6.3.SR1/eclim"
        eclimd-executable "/home/xgh/local/sts-bundle3.6.3/sts-3.6.3.SR1/eclimd"
        eclimd-default-workspace "~/workspace/"
        ))

;; call the help framework with the settings above & activate eclim-mode
;;(help-at-pt-set-timer)
;; add the emacs-eclim source
(require 'ac-emacs-eclim-source)
;;(ac-emacs-eclim-config)
;;(defadvice eclim-completion-start
;;  (around eclim-completion-start-when-dot activate)
;;  "Only use eclim to complete when encounter dot."
;;  (let ((dot-p (ac-prefix-c-dot)))
;;    (when dot-p
;;      ad-do-it
;;      dot-p)))

;; Hook eclim up with auto complete mode ---------------------------------------
;;(defun ac-prefix-eclim-java-dot ()
;;  "Do eclim-complete in auto-complete when encounter dot. To be compatible with
;;eclim--completion-action, call `eclim-completion-start' to set variable
;;`eclim--completion-start'."
;;  (let ((dot-p (ac-prefix-c-dot)))
;;		(when dot-p
;;		  (eclim-completion-start)
;;		  dot-p)))
;;
;;(ac-define-source eclim-java
;;  '((candidates . eclim--completion-candidates)
;;    (action . eclim--completion-action)
;;    (prefix . ac-prefix-eclim-java-dot)
;;    (requires . 0)
;;    (document . eclim--completion-documentation)
;;    (cache)
;;    (symbol . "f")))
;;(add-hook 'java-mode-hook
;;          (lambda ()
;;            (semantic-mode -1)
;;            (add-to-list 'ac-sources 'ac-source-eclim-java)))

(defcustom eclim-highlight-problems-modes '(java-mode)
  "Modes that use eclim toh highlight problems."
  :group 'eclim
  :type 'list)
;;(defadvice eclim-problems-highlight
;;  (around modes-run-eclim-problems-highlight activate)
;;  (if (memq major-mode eclim-highlight-problems-modes)
;;      ad-do-it))

;; mvn facility ----------------------------------------------------------------
(defun mvn (phase)
  "Run mvn using PHASE."
  (interactive (list (eclim--maven-lifecycle-phase-read)))
  (let ((pom-path (concat (projectile-project-root) "pom.xml")))
    (if (equal phase "")
        (setq phase (car eclim-maven-lifecycle-phases)))
    (if (file-exists-p pom-path)
        (compile (concat "mvn -f " pom-path " " phase))
      (message "Please go to mvn project and insure that \".projectile\" is in \
your project root path."))))
(setq compilation-error-regexp-alist 
      (delete
       '("^\\(.*\\):\\[\\([0-9]*\\),\\([0-9]*\\)\\]" 1 2 3)
       compilation-error-regexp-alist))

;; java-snippets A set of java-mode snippets for YASnippet. --------------------
(lambda-package-ensure-install 'java-snippets)

(provide 'lambda-java)

;;; lambda-java.el ends here
