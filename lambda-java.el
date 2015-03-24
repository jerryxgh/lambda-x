;;; lambda-java.el --- Java

;;; Commentary:

;;; Code:

(require 'lambda-core)
(require 'lambda-cc)

;; eclim plugin: using eclipse feature in emacs --------------------------------
(lambda-package-ensure-install 'emacs-eclim)
(require 'eclim)
(require 'eclimd)

;; start eclimd server in background
(setq eclimd-wait-for-process nil)

;; configuring eclipse installation
(when (eq system-type 'gnu/linux)
  (add-to-list 'eclim-eclipse-dirs
               "/home/xgh/local/sts-bundle/sts-3.6.4.RELEASE/")
  (setq eclim-executable "/home/xgh/local/sts-bundle/sts-3.6.4.RELEASE/eclim"
        eclimd-executable "/home/xgh/local/sts-bundle/sts-3.6.4.RELEASE/eclimd"
        eclimd-default-workspace "~/workspace/"))

;; displaying compilation error messages in the echo area
(setq help-at-pt-display-when-idle t
      help-at-pt-timer-delay 0.1)
(help-at-pt-set-timer)

;; configuring auto-complete-mode
(require 'ac-emacs-eclim-source)
(defun ac-prefix-eclim-java-dot ()
  "Do eclim-complete in auto-complete when encounter dot.

To be compatible with eclim--completion-action, call
`eclim-completion-start' to set variable
`eclim--completion-start'."
  (let ((dot-p (ac-prefix-c-dot)))
    (when dot-p
      (eclim-completion-start)
      dot-p)))

(ac-define-source eclim-java
  '((candidates . eclim--completion-candidates)
    (action . eclim--completion-action)
    (prefix . ac-prefix-eclim-java-dot)
    (document . eclim--completion-documentation)
    (cache)
    (selection-face . ac-emacs-eclim-selection-face)
    (candidate-face . ac-emacs-eclim-candidate-face)
    (requires . 0)
    (symbol . "f")))

(add-hook 'java-mode-hook
          #'(lambda ()
              (add-to-list 'ac-sources 'ac-source-eclim-java)))

;; configuring mvn
;; move compile to the first
(setq eclim-maven-lifecycle-phases (delete "compile"
                                           eclim-maven-lifecycle-phases))
(add-to-list 'eclim-maven-lifecycle-phases "compile")

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

;; to recognize mvn compile errors
(setq compilation-error-regexp-alist
      (delete
       '("^\\(.*\\):\\[\\([0-9]*\\),\\([0-9]*\\)\\]" 1 2 3)
       compilation-error-regexp-alist))

;; java-snippets A set of java-mode snippets for YASnippet. --------------------
(lambda-package-ensure-install 'java-snippets)

(provide 'lambda-java)

;;; lambda-java.el ends here
