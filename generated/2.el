;; -*-  lexical-binding: t; -*-
(defmacro if-let-key (key vars if-yes &rest if-no)
  `(let ((,(caar vars) ,(cadar vars)))
     (if (funcall ,key ,(caar vars))
	 ,(if(cdr vars)
	      (macroexpand-1 `(if-let-key ,key ,(cdr vars) ,if-yes ,@if-no))
	    if-yes)
       ,@if-no)))

(defmacro ifn-let-key (key vars if-yes &rest if-no)
  `(let ((,(caar vars) ,(cadar vars)))
     (ifn (funcall ,key ,(caar vars))
	 ,(if(cdr vars)
	      (macroexpand-1 `(if-let-key ,key ,(cdr vars) ,if-yes ,@if-no))
	    if-yes)
       ,@if-no)))

(defmacro if-failed(expr err-msg &rest body)
(let ((expr-result (s-gensym "ER")))
  `(ifn-let-key #'car ((,expr-result ,expr))

(let* ((because (concat "
because "

(cond
((keywordp (cdr ,expr-result)) (symbol-name (cdr ,expr-result)))
((stringp (cdr ,expr-result))  (cdr ,expr-result))
(t ""))))

(emsg (cons :error (cond
((stringp ,err-msg) (list (concat ,err-msg because)))
((listp ,err-msg) (cons(concat(car ,err-msg) because) (cdr ,err-msg)))
(t (list (concat "<invalid description>" because)))))))
(setf ok nil)

(cons (apply #'clog emsg) (apply #'format (cdr emsg))))
,@body)))

(defun get-file-properties* (FN)
  (when-let ((FA (and (file-exists-p FN) (file-attributes FN 'string)))
	     (DB-rec (make-vector (length file-fields) nil)))
      (destructuring-bind
	  (uid gid acess-time mod-time status-time fsize ms void inode fsNum)
	  (cddr FA)
	(aset DB-rec size fsize)
	(aset DB-rec gname gid)
	(aset DB-rec mtime mod-time); list of 4 integers
	(aset DB-rec modes (perms-from-str ms))
	(aset DB-rec plain (tilde FN)); (aset DB-rec write-me to-cloud); might be later adjusted in read-fileDB
	DB-rec)))

(defun get-file-properties (FN)
(when FN
  (or (cloud-locate-FN FN) (get-file-properties* (file-chase-links FN)))))

(defun cip-ext (FN)
"extension of encrypted file based on the original name"
(case* (file-name-extension FN) string=
       ("jpeg" ".png")
       ("jpg" ".png")
       (otherwise ".gpg")))

(defun forget-password(XYZ)
  "removes image password from password file"
(let* ((str (progn
	     (find-file (image-passes))
	     (buffer-string)))
       (BN (buffer-name)))
  (with-temp-file (image-passes)
    (insert (replace-regexp-in-string (format "%s .*
" XYZ) "" str)))
  (kill-buffer BN)))

(defmacro bad-column (cType N &optional str)
(if str
`(clog :error "invalid %dth column in %s line = %s" ,N ,cType ,str)
`(clog :error "invalid %dth column in %s line" ,N ,cType)))

(defun gpg-encrypt(FN XYZ)
(let* ((gpg-str (format "gpg --batch --yes --pinentry-mode loopback --passphrase %S -o %s --symmetric %s" password (concat (remote-directory) XYZ ".gpg") (untilde FN)))
       (result (shell-command gpg-str)))
(clog :debug gpg-str); for debugging, dangerous – shows passwords, to be erased!!!
(cons (= 0 result) result)))

(defun gpg-decrypt(FN XYZ)
(let* ((gpg-str (format "gpg --batch --yes --pinentry-mode loopback --passphrase %S -o %s --decrypt %s" password (untilde FN) (concat (remote-directory) XYZ ".gpg")))
       (result (shell-command gpg-str)))
(clog :debug gpg-str); for debugging, dangerous – shows passwords, to be erased!!!
(cons (= 0 result) result)))

(defun safe-dired-delete (FN)
  (condition-case err (cons t (funcall DDF FN "always"))
    (file-error
      (cons nil (clog :error "in DDF: %s" (error-message-string err))))))

(defun time< (t1 t2)
  (and
    (time-less-p (time-add t1 3) t2)
    (not (time-less-p (time-add t2 3) t1))))

(defun replace-file-ext(FN new-ext)
  "replacing file extension"
  (concat (file-name-sans-extension FN) "." new-ext))

(defun youngest(&rest FNs)
  (car (sort FNs #'file-newer-than-file-p)))
