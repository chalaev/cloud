(defun plain-name  (df)(aref df plain))
(defun cipher-name (df)(aref df cipher))

(defun get-file-properties* (FN)
  (when-let ((FA (and (file-exists-p FN) (file-attributes FN 'string)))
	     (DB-rec (make-vector (length file-fields) nil)))
      (destructuring-bind
	  (uid gid acess-time mod-time status-time size ms void inode fsNum)
	  (cddr FA)
	(aset DB-rec uname uid)
	(aset DB-rec gname gid)
	(aset DB-rec mtime mod-time); list of 4 integers
	(aset DB-rec modes (perms-from-str ms))
	(aset DB-rec plain FN); (aset DB-rec write-me to-cloud); might be later adjusted in read-fileDB
	DB-rec)))
  
(defun get-file-properties (FN)
  (or (cloud-locate-FN FN) (get-file-properties* FN)))

(defun cip-ext (FN)
"extension of encrypted file based on the original name"
(case* (file-name-extension FN) string=
       ("jpeg" ".png")
       ("jpg" ".png")
       (otherwise ".gpg")))

(defun forget-password(XYZ)
  "removes image password from password file"
(let* ((str (progn
	     (find-file (all-passes))
	     (buffer-string)))
       (BN (buffer-name)))
  (with-temp-file (all-passes)
    (insert (replace-regexp-in-string (format "%s .*
" XYZ) "" str)))
  (kill-buffer BN)))

(defmacro bad-column (cType N &optional str)
(if str
`(clog :error "invalid %dth column in %s line = %s" ,N ,cType ,str)
`(clog :error "invalid %dth column in %s line" ,N ,cType)))

(safe-mkdir "/tmp/cloud/")
(defun gpg-encrypt(FN XYZ)
(let ((tmp-gpg (concat "/tmp/cloud/" XYZ ".gpg")))
(ifn (= 0 (apply #'call-process
(append (list "gpg" nil nil nil)
(split-string (format "--batch --yes --pinentry-mode loopback --passphrase %S -o %s --symmetric %s"
    password tmp-gpg (untilda FN))))))
(setf ok (clog :error "failed to encrypt %s to %s!" (local/all) remote/files))
(rename-file tmp-gpg (concat (remote-dir) XYZ ".gpg") t) t)))

(defun gpg-decrypt(FN XYZ)
(let ((tmp-gpg (concat "/tmp/cloud/" XYZ ".gpg")))
  (copy-file (concat (remote-dir) XYZ ".gpg") tmp-gpg t)
  (ifn (= 0 (apply #'call-process
(append (list "gpg" nil nil nil)
(split-string (format "--batch --yes --pinentry-mode loopback --passphrase %S -o %s --decrypt %s"
		      password (untilda FN) tmp-gpg)))))
       (clog :error "failed to encrypt %s to %s!" (local/all) remote/files)
(safe-delete-file tmp-gpg) t)))

(defmacro directory-lock(DN &rest body)
  (let ((lock-file (gensym "LF")))
`(let ((,lock-file (concat ,DN (system-name))))
  (if (member (safe-mkdir ,DN) '(:permission nil))
  (clog :error "cannot cloud-sync -- unable to lock directory %s" ,DN) (write-region (TS (current-time)) nil ,lock-file)
,@body
(ifn (and (safe-delete-file ,lock-file) (safe-delete-dir ,DN))
(clog :error "can not unlock %s" ,DN)
t)))))
