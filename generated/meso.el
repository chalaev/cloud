(ert-deftest cloud-init()
   "just check that non-empty config file is created during the first run"
(one-virgin-host nil nil
  (should (progn
  (cloud-init remote-directory) 
(when-let ((FR (cloud-get-file-properties (concat emacs-d "cloud/" localhost "/config")))
           (FSize (aref (get-file-properties* (concat emacs-d "cloud/" localhost "/config")) size)))
    (clog :info "deftest cloud-init: config file size = %d bytes" FSize)
(< 100 FSize))))))

(ert-deftest read-write-conf()
   "testing cloud-init, read-conf, and write-conf"
(one-virgin-host nil nil (cloud-init)
(clog :info "read-write-conf: (local/host/conf) => %s" (local/host/conf))
(clog :info "
Here is the generated config file: ==>")
(with-temp-buffer (insert-file-contents (local/host/conf))
(while-let (str) (< 0 (length (setf str (read-line)))) (clog :info "%s" str)))
(clog :info "<== end of config file
")
(should (listp black-extensions))
(should (< 0 (length black-extensions)))
(should (land (mapcar #'stringp black-extensions)))
(should (listp black-root-dirs))
(should (< 0 (length black-root-dirs)))
(should (land (mapcar #'stringp black-root-dirs)))
(should (land (mapcar #'stringp (list remote/files remote-directory password))))
(let ((remote-directory "/mnt/remote/galaxy/")
      (black-extensions '("abc" "def"))
      (black-root-dirs '("/trash/"))
      (remote/files "QWERTY")
      (password "myDogsName"))
(write-conf))
(clog :info "
Here is my artificial config file: ==>")
(with-temp-buffer (insert-file-contents (local/host/conf))
  (while-let (str) (< 0 (length (setf str (read-line)))) (clog :info "%s" str)))
(clog :info "<== end of config file
")
(let((conf(read-conf)) remote-directory black-extensions black-root-dirs remote/files number-of-CPU-cores password remote-directory password)
(should conf)
(update-conf conf (split-string
"remote-directory black-extensions black-root-dirs remote/files number-of-CPU-cores password"))
(should (string= remote-directory "/mnt/remote/galaxy/"))
(should (equal black-extensions '("abc" "def")))
(should (equal black-root-dirs '("/trash/")))
(should (string= remote/files "QWERTY"))
(should (string= password "myDogsName")))))
