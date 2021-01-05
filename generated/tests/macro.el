;; -*- lexical-binding: t; -*-
(require 'ert)
(defun ert/when-passed()
  (when(functionp 'ert/on-success) (funcall #'ert/on-success)))
(add-function :before (symbol-function 'ert-pass) #'ert/when-passed)
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

(defvar host-par-names '(file-DB ~ HOME emacs-d password remote/files removed-files important-msgs gpg-process cloud-was-connected all Makefile uploaded stanze
   remote-directory dot-file dot-dir conf-file dir-1 file-1 file-1a file-2 file-3 file-4 file-5))
(eval (cons 'progn (mapcar #'(lambda(VD) `(defvar ,VD nil)) host-par-names)))
;;(define-vars (dot-file dot-dir conf-file dir-1 file-1 file-2 file-3 file-4 file-5))

(defmacro one-virgin-host(dirs &rest body)
"simulating first run on a new host -- in order to test config file creation;
dirs is a list (root-dir remote-dir), might be nil,
root-dir is (optional, might be nil) root directory for this host"
(let ((ds (s-gensym "ds")))
`(let* ((,ds ,dirs)
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
(file-1a  (concat ~ "file-1a.dat")); let us fix its name 
(dir-1 (to-dir(make-temp-file (concat ~ "dir-1.") t)))
(file-2 (make-temp-file (concat dir-1 "tmp-2.") nil nil (concat "file-2 " signature))); blacklisted
(file-3 (make-temp-file (concat dir-1 "file-3.") nil nil (concat "file-3 " signature)))
(file-4 (make-temp-file (concat dir-1 "file-4.") nil nil (concat "file-4 " signature))); to be encrypted
(file-5 (make-temp-file (concat dir-1 "file-5.") nil nil (concat "file-5 " signature))); to be gzipped
(host-conf (make-hash-table)))
(write-region (rand-str 3) nil file-1a)
(ert/home root-dir)
(clog :info "home directory(~)= %s, remote-directory= %s, local-dir= %s" ~ remote-directory (local-dir))
(ensure-dir-exists (local-dir))
(ensure-dir-exists (local/host/))
(setf password "12345"); comment this string to get random password
(cloud-init)
(dolist (CP host-par-names) (puthash CP (eval(intern(symbol-name CP))) host-conf))
 ,@body)))
(defmacro two-virgin-hosts(&rest body)
"simulating first run on a new host -- in order to test config file creation"
`(let (hostA hostB 
(root (to-dir(make-temp-file "cloud-test.PAIR-root." t)))
(remote (to-dir(make-temp-file "cloud-test.PAIR-remote." t))))
(one-virgin-host (list (concat root "A") remote)
(setf hostA host-conf)
(one-virgin-host (list (concat root "B") remote)
(setf hostB host-conf)

(dolist (CP '(password remote-directory remote/files))
  (set (intern(symbol-name CP)) (gethash CP hostA))
  (setf (gethash CP hostB)  (gethash CP hostA)))
(write-conf)
,@body))))

(defmacro on-hostA(&rest body)
`(let ,host-par-names
(dolist (CP (quote (list ,@host-par-names)))
    (set (intern(symbol-name CP)) (gethash CP hostA)))
(clog :info "host A> remote/files= %s, password= %s, HOME= %s" remote/files password HOME)
,@body))
(defmacro on-hostB(&rest body)
`(let ,host-par-names
(dolist (CP (quote (list ,@host-par-names)))
    (set (intern(symbol-name CP)) (gethash CP hostB)))
(clog :info "host B> remote/files= %s, password= %s, HOME= %s" remote/files password HOME)
,@body))

(ert-deftest cloud-sync-1()
"copying one file from hostA to hostB"
(two-virgin-hosts
(let (FN CF)
(on-hostA
  (setf FN (tilde file-1) CF (cat-file (untilde file-1)))
  (cloud-start)
  (cloud-add file-1)
  (cloud-sync))
(on-hostB
  (should (not (file-exists-p (untilde FN))))
  (cloud-start); cloud-start calls cloud-sync that has downloaded file-1 from remote directory
  (should (file-exists-p (untilde file-1))); so now file-1 exists on-hostB
  (should (string= CF (cat-file (untilde FN))))))))

(ert-deftest cloud-sync-2()
"copying one file from hostA to hostB"
(two-virgin-hosts
(let (CF)
(on-hostA
  (setf CF (cat-file (untilde file-1a)))
  (cloud-start)
  (clog :info "touch 'now + 5 sec' %s" file-1a) 
  (set-file-times (untilde file-1a) (time-add (current-time) 5))
  (cloud-add file-1a)
  (cloud-sync))
(on-hostB
  (should (file-exists-p file-1a)))
  (should(not(string= CF (cat-file (untilde file-1a))))); files file-1a on two hosts have the same name, but different content
  (cloud-start); cloud-start calls cloud-sync that has downloaded file-1a from remote directory
  (should (string= CF (cat-file (untilde file-1a)))))))
