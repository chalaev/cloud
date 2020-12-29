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
