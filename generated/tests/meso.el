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
(ert-deftest cloud-and-upload()
"clouding a file and uploading it to remote directory"
(one-virgin-host nil
(should (file-exists-p(untilde file-1)))
(cloud-add file-1)
(let((remote-files-1(directory-files remote-directory nil "...\...." t)))
(clog :info "before syncying there are %d files in the remote diredtory: %s" (length remote-files-1) (together remote-files-1))
(cloud-sync)
(let((remote-files-2(directory-files remote-directory nil "...\...." t)))
(clog :info "after syncying there are %d files in the remote diredtory: %s" (length remote-files-2) (together remote-files-2))
(should (< (length remote-files-1) (length remote-files-2)))
(clog :info "will now cloud %s and sync" file-2)
(should (file-exists-p(untilde file-2)))
(cloud-add file-2)
(cloud-sync)
(let((remote-files-3(directory-files remote-directory nil "...\...." t)))
(clog :info "after syncying there are %d files in the remote diredtory: %s" (length remote-files-3) (together remote-files-3))
(should (< (length remote-files-2) (length remote-files-3))))))))

(defun file-mtime(FN &optional t0)
(when(file-exists-p FN)
  (round(float-time(time-subtract(aref(get-file-properties* FN) mtime) (or t0 '(0 0)))))))

(ert-deftest save-then-upload()
"uploading previously updated file that was clouded before"
(one-virgin-host nil
(should(file-exists-p(untilde file-1a)))
(cloud-add file-1a)
(let((FR(cloud-locate-FN file-1a)))
(should FR)
(let((t0(aref FR mtime)) (CN(aref FR cipher)))
(should CN)
(let((gpg-FN (concat(remote-directory) CN ".gpg")))
(should(not(file-exists-p gpg-FN)))
(sleep-for 1) (cloud-sync)
(should(file-exists-p gpg-FN))
(let((t1 (file-mtime gpg-FN t0))); all times are relative to t0
(should(< 0 t1))
(clog :info "touch 'now + 5 sec' %s" file-1a) (set-file-times (untilde file-1a) (time-add (current-time) 5))
(sleep-for 1)
(touch (untilde file-1a)); as if we saved the changes to file-1a in emacs
(let((t2 (file-mtime file-1a t0)))
(should(< t1 t2)); 12/28 fails
(cloud-sync)
(let((t3 (file-mtime gpg-FN t0)))
(should(< t1 t3))))))))))
(ert-deftest cloud-init()
   "just check that non-empty config file is created during the first run"
(one-virgin-host nil
  (should (progn
  (cloud-init remote-directory) 
(when-let ((FR (cloud-get-file-properties (concat emacs-d "cloud/" localhost "/config")))
           (FSize (aref (get-file-properties* (concat emacs-d "cloud/" localhost "/config")) size)))
    (clog :info "deftest cloud-init: config file size = %d bytes" FSize)
(< 100 FSize))))))

(ert-deftest read-write-conf()
   "testing cloud-init, read-conf, and write-conf"
(one-virgin-host nil (cloud-init)
(clog :info "read-write-conf: (local/host/conf) => %s" (local/host/conf))
(clog :info "
Here is the generated config file: ==>")
(with-temp-buffer (insert-file-contents (local/host/conf))
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
(with-temp-buffer (insert-file-contents (local/host/conf))
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
