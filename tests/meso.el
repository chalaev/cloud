;; -*-  lexical-binding: t; -*-
;; tests/meso.el
(ert-deftest read-write-conf()
   "testing cloud-init, read-conf, and write-conf"
(debug-environment (host> (car hostnames)
(let((tmp-conf(concat local/host/conf ".tmp")))
(debug-log-var tmp-conf)
(copy-file local/host/conf tmp-conf)
(debug-log-var file-1); file-1= ~/file-1.hostA
(clog :info "read-write-conf: tmp-conf => %s" tmp-conf)
(with-temp-buffer(insert-file-contents tmp-conf)
(clog :info "
Here is the auto-generated (by cloud.el) config file: ==>
%s <== end of config file
" (buffer-string)))
(letc nil
((remote-directory remote-directory); do not change this value
 (black-extensions '("abc" "def"))
 (black-root-dirs '("/trash/"))
 (remote/files  "XYZ")
 (password "myDogsName"))
(write-conf tmp-conf
  (make-conf remote-directory black-extensions black-root-dirs remote/files password)))
(with-temp-buffer(insert-file-contents tmp-conf)
(clog :info "
Here is my artificial config file: ==>
%s <== end of config file
" (buffer-string)))
(letc(read-conf-file tmp-conf)
(((:string) black-extensions) ((:string) black-root-dirs)
(:string remote/files) (:integer number-of-CPU-cores)
(:string password) (:string remote-directory))
(clog :debug "test read-write-conf> remote-directory=> %s, black-extensions=> %s" remote-directory black-extensions)
(should (equal black-extensions '("abc" "def")))
(should (equal black-root-dirs '("/trash/")))
(should (string= remote/files "XYZ"))
(should (string= password "myDogsName")))))))

(ert-deftest cloud-init()
   "just check that a non-empty config file is created during the first run"
(debug-environment (host> (car hostnames)
(let((dir(untilde remote-directory)))
(should(string-prefix-p "/tmp/" dir))
;;(cloud-init)
(indices
(when-let((FR (cloud-get-file-properties local/host/conf))
          (FSize (aref (get-file-properties* local/host/conf) size)))
    (clog :info "deftest cloud-init: config file size = %d bytes,
and its content is
%s" FSize (cat-file local/host/conf))
  (should (< 50 FSize))))))))

(ert-deftest cloud-and-upload()
  "clouding a file and uploading it to remote directory"
(debug-environment (host> (car hostnames)
  ;;(cloud-init)
  (should(file-exists-p(untilde file-1)))
  (cloud-add file-1)
  (let((remote-files-1(directory-files remote-directory nil "...\...." t)))
    (clog :info "before syncying there are %d files in the remote directory: %s" (length remote-files-1) (together remote-files-1)); before syncying there are 0 files in the remote directory
    (cloud-sync)
    (let((remote-files-2(directory-files remote-directory nil "...\...." t)))
      (clog :info "after syncying there are %d files in the remote directory: %s" (length remote-files-2) (together remote-files-2))
      (should(< (length remote-files-1) (length remote-files-2)))
      (clog :info "will now cloud %s and sync" file-2)
      (should(file-exists-p(untilde file-2)))
      (cloud-add file-2)
      (cloud-sync)
(let((remote-files-3(directory-files remote-directory nil "...\...." t)))
  (clog :info "after syncying there are %d files in the remote directory: %s" (length remote-files-3) (together remote-files-3))
  (should(< (length remote-files-2) (length remote-files-3)))))))))

(defun file-mtime(FN &optional t0)
(when(file-exists-p FN)
(indices
  (round(float-time(time-subtract(aref(get-file-properties* FN) mtime) (or t0 '(0 0))))))))

(ert-deftest save-then-upload()
  "uploading previously updated file that was clouded before"
(skip-unless nil)
(debug-environment (host> (car hostnames)
;;(cloud-init)
(indices
(load (FN debug-make-dir "generated/functions.el"))
  (should(file-exists-p(untilde file-1a)))
  (cloud-add file-1a)
  (let((FR(cloud-locate-FN file-1a)))
    (should FR)
    (let((t0(aref FR mtime)) (CN(aref FR cipher)))
      (should CN)
      (let((gpg-FN (concat remote-directory CN ".gpg")))
	(should(not(file-exists-p gpg-FN)))
	(sleep-for 1) (cloud-sync)
	(should(file-exists-p gpg-FN))
	(let((t1 (file-mtime gpg-FN t0))); all times are counted from t0
	  (should(< 0 t1))
	  (clog :info "touch 'now + 5 sec' %s" file-1a) (set-file-times (untilde file-1a) (time-add (current-time) 5))
	  (sleep-for 1)
	  (cloud-touch (untilde file-1a)); as if we saved the changes to file-1a in emacs
	  (let((t2 (file-mtime file-1a t0)))
	    (should(< t1 t2))
	    (cloud-sync)
	    (let((t3(file-mtime gpg-FN t0)))
	      (should(< t1 t3))))))))))))
