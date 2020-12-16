(defvar host-par-names '(~ emacs-d removed-files important-msgs gpg-process cloud-was-connected all Makefile uploaded stanze
   remote-directory dot-file dot-dir conf-file dir-1 file-1 file-2 file-3 file-4 file-5))
(defmacro one-virgin-host(&rest body)
"simulating first run on a new host -- in order to test config file creation"
`(let* ((~ (file-name-as-directory (make-temp-file "cloud-test.home." t)))
        (emacs-d (file-name-as-directory (make-temp-file (concat ~ ".emacs.d.") t)))
        (remote-directory (file-name-as-directory (make-temp-file "cloud-test-mnt.remote." t)))

(dot-file (make-temp-file (concat ~ ".bash-config.") nil nil "dot-file; it is blacklisted"))
(dot-dir (file-name-as-directory (make-temp-file (concat ~ ".config-dir.") t))); all files inside =dot-dir= are blacklisted
(conf-file (make-temp-file (concat dot-dir "file.") nil nil "conf-file; it is blacklisted"))

(file-1 (make-temp-file (concat ~ "file-1.") nil nil "file-1"))
(dir-1 (file-name-as-directory (make-temp-file (concat ~ "dir-1.") t)))
(file-2 (make-temp-file (concat dir-1 "tmp-2.") nil nil "file-2")); blacklisted
(file-3 (make-temp-file (concat dir-1 "file-3.") nil nil "file-3"))
(file-4 (make-temp-file (concat dir-1 "file-4.") nil nil "file-4")); to be encrypted
(file-5 (make-temp-file (concat dir-1 "file-5.") nil nil "file-5")); to be gzipped
(host-conf (make-hash-table)))
(dolist (CP host-par-names) (setf (gethash CP host-conf) (eval(intern(symbol-name CP)))))
(clog :info "home directory = ~ = %s" ~)
(clog :info "remote-directory= %s" remote-directory)
(clog :info "local-dir= %s" (local-dir))
(unless (ensure-dir-exists (local-dir)) (clog :error "fatal: cannot create %s" (local-dir)))
;;(clog :debug "ensure-dir-exists(%s)" (local/host/))
(ifn (ensure-dir-exists (local/host/)) (clog :error "cannot create %s" (local/host/))
(clog :info "Starting new test environment in the directory %s
=======" emacs-d)
(prog1 (progn ,@body)
(clog :info "======
cleanig test environment in the directory %s
" emacs-d)
(safe-delete-dir remote-directory)
(safe-delete-dir emacs-d)
(safe-delete-dir ~)))))
(defmacro two-virgin-hosts(&rest body)
"simulating first run on a new host -- in order to test config file creation"
`(let (hostA hostB)
(one-virgin-host (cloud-init)
(setf hostA host-conf)
(ifn password
 (clog :error "failed to generate password for hostA!")
(clog :info "generated password %s for hostA" password)
(dolist (CP (cons 'password host-par-names))
    (setf (gethash CP hostA) (eval(intern(symbol-name CP)))))
(one-virgin-host(cloud-init)
(setf hostB host-conf)
(ifn password
 (clog :error "failed to generate password for hostB!")
(clog :info "generated password %s for hostB" password)
(dolist (CP (cons 'password host-par-names))
    (setf (gethash CP hostB) (eval(intern(symbol-name CP)))))

(let ((password "12345678"))
,@body)))))))

(defmacro on-hostA(&rest body)
`(let ,host-par-names
(dolist (CP (quote (list ,@host-par-names)))
    (set (intern(symbol-name CP)) (gethash CP hostA)))
,@body))
(defmacro on-hostB(&rest body)
`(let ,host-par-names
(dolist (CP (quote (list ,@host-par-names)))
    (set (intern(symbol-name CP)) (gethash CP hostB)))
,@body))

(ert-deftest cloud-sync()
"creating fresh configuration on 2 hosts and copying one file"
(two-virgin-hosts
 (on-hostA
  (clog :info "clouding %s..." file-1)
  (cloud-add file-1)
  (clog :info "after clouding %s, Makefile is
%s" file-1 (apply #'concat (reverse Makefile)))
  (clog :info "uploading %s..." file-1)
(cloud-sync))
(on-hostB
 (clog :info "downloading %s..." file-1)
 (cloud-sync)
(should (file-exists-p file-1))
 (ifn (file-exists-p file-1) (clog :error "file %s was not downloaded!" file-1)
 (clog :info "file %s was downloaded" file-1)
(with-temp-buffer (safe-insert-file file-1)
(should (string= "file-1" (read-line))))))))
