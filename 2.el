;; Time-stamp: <2020-10-04 11:19 MSK>

;; Note sure if the following 2 functions are necessary, or, may be, they should be declared as macro or "inline":
(defun plain-name  (df)(aref df plain))
(defun cipher-name (df)(aref df cipher))

(defun add-files(&rest names)
  (let ((ok t))
    (dolist (FN names)
      (clog :debug "add-files(%s)" FN)
      (unless (cloud-locate-FN FN)
	(needs ((GFP (get-file-properties FN) (clog :error "Invalid attempt to cloud inexisting file %s" FN)))
	       (aset GFP cipher (new-file-name *cloud-dir*))
	       (setf ok (and ok GFP))
	       (push GFP *file-DB*))))
    ok))

;; Кстати, =(file-exists-p FN)= выдаст ~nil~ в случае, если файл находится в запрещённом для чтения (rx) каталоге →
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
