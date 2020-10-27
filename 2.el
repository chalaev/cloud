;; Time-stamp: <2020-10-27 13:35 EDT>

;; Note sure if the following 2 functions are necessary, or, may be, they should be declared as macro or "inline":
(defun plain-name  (df)(aref df plain))
(defun cipher-name (df)(aref df cipher))

(defun add-files(&rest names)
  (let ((ok t))
    (dolist (FN names)
      (clog :debug "add-files(%s)" FN)
      (unless (cloud-locate-FN FN)
	(needs ((GFP (get-file-properties FN) (clog :error "Invalid attempt to cloud inexisting file %s" FN))
		(CN (new-file-name cloud-dir)))
	       (aset GFP cipher CN)
	       (setf ok (and ok GFP))
	       (push GFP file-DB)
(when (member (file-name-extension FN) '("jpeg" "png" "jpg"))
(write-region
(format "%s %s
" CN (rand-str 16)) nil (all-passes) t))
(upload GFP))))
    ok))

;; BTW, =(file-exists-p FN)= produces ~nil~ if the file resides in directory with (rx) permissions →
(defun get-file-properties (FN)
(or (cloud-locate-FN FN)
(when (file-exists-p FN)
  (when-let ((FA (file-attributes FN 'string)))
    (let ((DB-rec (make-vector (length DB-fields) nil)))
      (destructuring-bind
	  (uid gid acess-time mod-time status-time size ms void inode fsNum)
	  (cddr FA)
	(aset DB-rec uname uid)
	(aset DB-rec gname gid)
	(aset DB-rec mtime mod-time); list of 4 integers
	(aset DB-rec modes (perms-from-str ms))
	(aset DB-rec plain FN); (aset DB-rec write-me to-cloud); might be later adjusted in read-fileDB
	DB-rec))))))

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
