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
(ert-deftest cloud-init()
   "just check that non-empty config file is created during the first run"
(one-virgin-host
  (should (progn
  (cloud-init remote-directory) 
(when-let ((FR (get-file-properties (concat emacs-d "cloud/" localhost "/config")))
           (FSize (aref (get-file-properties (concat emacs-d "cloud/" localhost "/config")) size)))
    (clog :info "deftest cloud-init: config file size = %d bytes" FSize)
(< 100 FSize))))))

(ert-deftest read-write-conf()
   "testing cloud-init, read-conf, and write-conf"
(one-virgin-host (cloud-init) 
(clog :info "read-write-conf: (local/host/conf) => %s" (local/host/conf))
(clog :info "
Here is the generated config file: ==>")
(with-temp-buffer (safe-insert-file (local/host/conf))
(while-let (str) (< 0 (length (setf str (read-line)))) (clog :info "%s" str)))
(clog :info "<== end of config file
")
(should (listp junk-extensions))
(should (< 0 (length junk-extensions)))
(should (land (mapcar #'stringp junk-extensions)))
(should (listp ignored-dirs))
(should (< 0 (length ignored-dirs)))
(should (land (mapcar #'stringp ignored-dirs)))
(should (land (mapcar #'stringp (list remote/files remote-directory password))))

(let ((junk-extensions '("abc" "def"))
	(ignored-dirs '("/trash/"))
	(remote/files "QWERTY")
	(remote-directory "/mnt/remote/galaxy/")
	(password "myDogsName"))
(write-conf))
(clog :info "
Here is my artificial config file: ==>")
(with-temp-buffer (safe-insert-file (local/host/conf))
  (while-let (str) (< 0 (length (setf str (read-line)))) (clog :info "%s" str)))
(clog :info "<== end of config file
")
(ifn-let ((conf (read-conf))) (clog :error "(read-conf) failed")
(let (junk-extensions ignored-dirs remote/files remote-directory password)
(update-conf conf "remote-directory" "junk-extensions" "ignored-dirs" "remote/files" "number-of-CPU-cores" "password")
(should (equal junk-extensions '("abc" "def")))
(should (equal ignored-dirs '("/trash/")))
(should (string= remote/files "QWERTY"))
(should (string= remote-directory "/mnt/remote/galaxy/"))
(should (string= password "myDogsName"))))))
