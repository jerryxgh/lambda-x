;;; package --- jsonrpc_test -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'ert)

(ert-deftest json-test()
  "Json test."
  (should
   (let* ((id 1001)
          (params '((prefer_scan_login . t)))
          (method "auth")
          (req `((id . ,id) (jsonrpc . "2.0") (method . ,method) (params . ,params)))
          (json (json-encode req))
          (tick (buffer-chars-modified-tick)))
     (message json))))

(ert-deftest make-process-test()
  "Json test."
  (should
   (let* (
          (proc (make-process
                 :name "codeverse-jsonrpc"
                 ;; :buffer nil
                 ;; :command (list "/Users/bytedance/repository/public/lambda-x/misc/jsonrpc_test/auth-for-vim" "--logFile" "true")
                 :command (list "/Users/bytedance/.emacs.d/trae/bin/auth-for-vim-macos-arm64-1.0.0.53-91899ea78871dd4f5e7ffc08a10e9458" "--logFile" "true")
                 :noquery t
                 :stderr (get-buffer-create "*trae region stderr*")
                 :connection-type 'pipe
                 :coding 'utf-8-emacs-unix
                 ))
          (id 1)
          (params '((prefer_scan_login . nil)))
          (method "auth")
          (req `((id . ,id) (jsonrpc . "2.0") (method . ,method) (params . ,params)))
          (json (json-encode req))
          (tick (buffer-chars-modified-tick))
          (payload (format "Content-Length: %d\r\n\r\n%s" (length json) json))
          )
     (set-process-query-on-exit-flag proc nil)
     (set-process-filter proc (lambda (p s) (message "process-filter,p:%S,s:%S" p s)))
     (set-process-sentinel proc (lambda (p e) (message "process-sentinel,p:%S,e:%S" p e)))

     (message "filter:%S" (process-filter proc))
     (message "payload:%S" payload)
     (process-send-string proc payload)
     ;; (message "process-live-p:%S" (process-live-p proc))
     ;; (kill-process proc) ;; clean
     ;; (message "process-live-p:%S" (process-live-p proc))
     t
     )))


;; (make-process :name "trae auth"
;;               :command "./auth-for-vim --logFile true"
;;               :coding 'utf-8-emacs-unix
;;               :connection-type 'pipe
;;               :stderr (get-buffer-create "*trae auth stderr*")
;;               :noquery t)

;; (defun codeverse--jsonrpc-open (bin opts)
;;   "Start a JSON-RPC process BIN with OPTS handlers; return a state plist."
;;   (let* ((proc (make-process :name "codeverse-jsonrpc" :buffer nil :command (list bin "--logFile" "true") :coding 'no-conversion))
;;          (state (list (cons 'proc proc) (cons 'id 0) (cons 'stdout "") (cons 'content-length -1) (cons 'request-ctx (make-hash-table :test 'eql)))))
;;     (set-process-query-on-exit-flag proc nil)
;;     (set-process-filter proc (lambda (p s) (codeverse--on-stdout state p s opts)))
;;     (set-process-sentinel proc (lambda (p e) (codeverse--on-exit state p e opts)))
;;     state))

(provide 'jsonrpc_test)
;;; jsonrpc_test.el ends here
