(defun delete-dirs (&rest dirs)
 (mapcar #'(lambda(DN) (delete-directory DN t)) dirs))
(let (tmp-dirs)
(defun ert/home(&rest dirs)
"setting list of temporary directories"
(if(member (to-dir(getenv "HOME")) dirs) (clog :error "WTF? Home directory is used in the test!")
 (setf tmp-dirs dirs)))
(defun ert/on-success()
"cleaning temporary directories"
(ifn tmp-dirs (clog :error "tmp-dirs unset in ert/on-success")
(let ((CDs (together tmp-dirs)))
 (clog :info "cleaning up %s" CDs)
(if(member (to-dir(getenv "HOME")) tmp-dirs) (clog :error "STOP: home directory is used in the test!");  YES, I need this precaution!!!
 (clog :info "erasing %s" CDs)
 (apply #'delete-dirs tmp-dirs))))))

(defvar host-par-names '(localhost file-DB ~ HOME emacs-d password remote/files removed-files important-msgs gpg-process cloud-was-connected all Makefile uploaded stanze
   remote-directory dot-file dot-dir conf-file dir-1 file-1 file-1a dir-1a file-2 file-3 file-4 file-5))
(eval (cons 'progn (mapcar #'(lambda(VD) `(defvar ,VD nil)) host-par-names)))
;;(define-vars (dot-file dot-dir conf-file dir-1 file-1 file-2 file-3 file-4 file-5))

(defvar localhost-counter 0)
(defmacro one-virgin-host(LH dirs &rest body)
"simulating first run on a new host -- in order to test config file creation;
dirs is a list (root-dir remote-dir), might be nil,
root-dir is (optional, might be nil) root directory for this host"
(let ((ds (s-gensym "ds")))
`(let* ((,ds ,dirs)
   (localhost (or ,LH (format "%s-%d" localhost (incf localhost-counter))))
   (root-dir (to-dir (or (car ,ds) (make-temp-file "cloud-test.root." t))))
   (~ (need-dir root-dir "user")) (HOME (directory-file-name ~))
   (/tmp/cloud-test/ (need-dir (make-temp-file "cloud." t)))
   (remote-directory (or
      (and (cdr ,ds) (cadr ,ds))
      (to-dir root-dir "remote")))
(emacs-d (to-dir ~ ".emacs.d"))
(signature (format "
%s" (rand-str 3))); distinguishes files having the same name, but residing on different hosts

(dot-file (make-temp-file (concat ~ ".bash-config.") nil nil (concat "dot-file; it is blacklisted" signature)))
(dot-dir (to-dir (make-temp-file (concat ~ ".config-dir.") t))); all files inside =dot-dir= are blacklisted
(conf-file (make-temp-file (concat dot-dir "file.") nil nil "conf-file; it is blacklisted"))

(file-1 (tilde(make-temp-file (concat ~ "file-1.") nil nil (concat "file-1 " signature))))
(file-1a (concat ~ "file-1a.dat")); let us fix its name 
(dir-1 (to-dir(make-temp-file (concat ~ "dir-1.") t)))
(dir-1a (concat ~ "dir-1a")); let us fix its name 
(file-2 (make-temp-file (concat dir-1 "tmp-2.") nil nil (concat "file-2 " signature))); blacklisted
(file-3 (make-temp-file (concat dir-1 "file-3.") nil nil (concat "file-3 " signature)))
(file-4 (make-temp-file (concat dir-1 "file-4.") nil nil (concat "file-4 " signature))); to be encrypted
(file-5 (make-temp-file (concat dir-1 "file-5.") nil nil (concat "file-5 " signature))); to be gzipped
(host-conf (make-hash-table)))
(write-region (rand-str 3) nil file-1a)
(make-directory (untilde dir-1a) t)
(ert/home root-dir)
(clog :info "home directory(~)= %s, remote-directory= %s, local-dir= %s" ~ remote-directory (local-dir))
(ensure-dir-exists (local-dir))
(ensure-dir-exists (local/host/))
(setf password "12345"); comment this string to get random password
(cloud-init)
(dolist (CP host-par-names) (puthash CP (eval(intern(symbol-name CP))) host-conf))
 ,@body)))
