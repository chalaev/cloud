;; ert.el
(defun ert/on-success()
  "cleaning temporary directories"
  (ifn root-test-dir (clog :error "root-test-dir unset in ert/on-success")
    (if(string= root-test-dir (file-name-as-directory(getenv "HOME"))) (clog :error "WTF? Home directory is used in the test!")

(clog :info "erasing %s" root-test-dir)
    (delete-directory root-test-dir t))))
(defun ert/when-passed()
  (when(functionp 'ert/on-success) (funcall #'ert/on-success)))
(add-function :before (symbol-function 'ert-pass) #'ert/when-passed)
