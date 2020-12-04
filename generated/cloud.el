;; -*- mode: Emacs-Lisp;  lexical-binding: t; -*-
;; -*- mode: Emacs-Lisp;  lexical-binding: t; -*-
;; generated from https://notabug.org/shalaev/lisp-goodies/src/master/shalaev.org
;; Some day this file will probably replace standard cl.el in my projects
(let ((counter 0))
  (defun gensym(&optional starts-with)
    "for those who miss gensym from Common Lisp"
    (unless starts-with (setf starts-with "gs"))
    (let (sym)
      (while (progn
               (setf sym (make-symbol (concat starts-with (number-to-string counter))))
               (or (special-form-p sym) (functionp sym) (macrop sym) (boundp sym)))
        (incf counter))
      (incf counter)
      sym)))

(defun s-find(item seq &optional key test)
  (let ((test (or test #'=)))
    (dolist (CS seq)
      (when (funcall test item (if key (funcall key CS) CS))
	(return CS)))))

(unless (or (boundp 'decf) (functionp 'decf) (macrop 'decf))
(defmacro decf (var &optional amount)
  (unless amount (setf amount 1))
  `(setf ,var (- ,var ,amount))))

(unless (or (boundp 'incf) (functionp 'incf) (macrop 'incf))
(defmacro incf (var &optional amount)
  (unless amount (setf amount 1))
  `(setf ,var (+ ,var ,amount))))

(defmacro flet(fun-defs &rest body)
(let ((GSs (mapcar #'(lambda(FD) (cons (car FD) (gensym))) fun-defs)))
`(let ,(mapcar #'(lambda(FD)
(list (cdr (assoc (car FD) GSs))
`(lambda ,(cadr FD) ,@(cddr FD)))) fun-defs)
(macrolet ,(mapcar #'(lambda(FD)
(list (car FD) (cadr FD) `(funcall ,(cdr (assoc (car FD) GSs)) ,@(cadr FD)))) fun-defs)
 ,@body))))

(defun without(source &rest wrong-items)
  "returns (copy of) source without wrong-items"
  (car (select source #'(lambda(x) (not (member x wrong-items))))))
(defmacro string-from-macro(m)
`(format "%s" (print (macroexpand-1 ,m) #'(lambda(x) (format "%s" x)))))

(defmacro when-let (vars &rest body)
  "when with let using standard let-notation"
  (if (caar vars)
  `(let ((,(caar vars) ,(cadar vars)))
     ,(if (cdr vars)
	  `(when ,(caar vars)
	     ,(macroexpand-1 `(when-let ,(cdr vars) ,@body)))
	(append `(when ,(caar vars)) body)))
  (if (cdr vars)
      `(when ,(cadar vars)
	     ,(macroexpand-1 `(when-let ,(cdr vars) ,@body)))
    (append `(when ,(cadar vars)) body))))

(defmacro if-let (vars ifyes &rest body)
  "if with let using standard let-notation"
  (let ((if-true (gensym "it")) (result (gensym "r")))
    `(let (,if-true ,result)
       (when-let ,vars
		 (setf ,if-true t
		  ,result ,ifyes))
       (if ,if-true
	   ,result
	 ,@body))))

(defmacro ifn-let (vars ifno &rest body)
  `(if-let ,vars
      (progn ,@body)
      ,ifno))

(defmacro needs (vardefs &rest body)
  "unifying when-let and if-let"
  (let ((vardef (car vardefs)))
    (if (and (listp vardef) (not (or (special-form-p (car vardef)) (functionp (car vardef)) (macrop (car vardef)))))
    `(let ((,(car vardef) ,(cadr vardef)))
       ,(if (cddr vardef)
	    `(if ,(car vardef)
		,(if (cdr vardefs)
		     (macroexpand-1 `(needs ,(cdr vardefs) ,@body))
		   `(progn ,@body))
	       ,(car (cddr vardef)))
	  (append `(when ,(car vardef))
		  (if (cdr vardefs)
		      (list (macroexpand-1 `(needs ,(cdr vardefs) ,@body)))
		    body))))
    (append `(when ,vardef)
		  (if (cdr vardefs)
		      (list (macroexpand-1 `(needs ,(cdr vardefs) ,@body)))
		    body)))))

(defmacro first2(ll) `(firstN ,ll 2))
(defmacro needs-set (vardefs &rest body)
  "needs with 'let' being replaced with 'setf'"
  (let ((vardef (car vardefs)))
    (if (cddr vardef)
      `(if-set (,(first2 vardef))
	  ,(if (cdr vardefs)
	       (macroexpand-1 `(needs-set ,(cdr vardefs) ,@body))
	     `(progn ,@body))
	  ,(caddr vardef))
      `(when-set (,(car vardefs))
	   ,(if (cdr vardefs)
	       (macroexpand-1 `(needs-set ,(cdr vardefs) ,@body))
	      `(progn ,@body))))))

(defmacro directory-lock(locked-dir by &rest body)
(let ((LD (gensym "LD")) (lock-file (gensym "LF")) (mkdir (gensym "MD")) (result (gensym "r")) (unlock (gensym "u")))
`(let* ((,LD (file-name-as-directory ,locked-dir))
        (,lock-file (concat ,LD "by"))
        (,mkdir (safe-mkdir ,LD)))
  (ifn (car ,mkdir) (cons nil (cons :lock ,mkdir))
  (write-region ,by nil ,lock-file)
  (let ((,result (progn ,@body)))
    (if-let ((,unlock (and (rm ,lock-file) (safe-delete-dir ,LD))))
      (cons t ,result)
      (cons nil (cons :unlock (cons ,unlock ,result)))))))))

(defmacro drop (from-where &rest what)
`(setf ,from-where (without ,from-where ,@what)))

(defmacro define-vars (varDefs)
  "to make switching between local/global variables easier"
(cons 'progn
(mapcar #'(lambda(VD)
  (if (consp VD)
      `(defvar ,@VD)
      `(defvar ,VD nil)))
varDefs)))

;; -*- mode: Emacs-Lisp;  lexical-binding: t; -*-
;; generated from https://notabug.org/shalaev/lisp-goodies/src/master/shalaev.org
(defmacro case* (expr test &rest cases)
  "case with arbitrary test function"
  (let ((v (gensym "v")))
    `(let ((,v ,expr))
       (cond
        ,@(mapcar #'(lambda (VR)
(let ((val (car VR)) (rest (cdr VR)))
  (if (eql val 'otherwise)
      `(t ,@rest)
    `((,test ,v ,val) ,@rest))))
 cases)))))

(defmacro when-set (vars &rest body)
  "when-let using global variable instead of defining local one"
(let ((GV (gensym)))
  `(let ((,GV ,(cadar vars)))
     ,(if (cdr vars)
	  `(when ,GV
              (setf ,(caar vars) ,GV)
	     ,(macroexpand-1 `(when-set ,(cdr vars) ,@body)))
	(append `(when ,GV (setf ,(caar vars) ,GV)) body)))))

(defmacro unless-set (vars &rest body)
  "unless-let using global variable instead of defining local one"
(let ((GV (gensym)))
  `(let ((,GV ,(cadar vars)))
     ,(if (cdr vars)
	  `(if ,GV
              (setf ,(caar vars) ,GV)
	     ,(macroexpand-1 `(unless-set ,(cdr vars) ,@body)))
	(append `(if ,GV (setf ,(caar vars) ,GV)) body)))))

(defmacro if-set (vars &rest body)
  (let ((if-true (gensym "it")) (result (gensym "r")))
    `(let (,if-true ,result)
       (setf ,result (when-set ,vars
		  (setf ,if-true t)
		  ,(car body)))
       (if ,if-true ,result
	 ,@(cdr body)))))

(defmacro ifn-set (vars ifno &rest body)
`(if-set ,vars
   (progn ,@body)
   ,ifno))

(defmacro cond-let (&rest conds)
  "cond with let"
  (let ((c (car conds)) (r (cdr conds)))
    (if (equal (car c) 'otherwise) `(progn ,@(cdr c))
    (if r
	`(if-let ,(car c) (progn ,@(cdr c)) ,(macroexpand-1 `(cond-let ,@r)))
	`(when-let ,(car c) ,@(cdr c))))))

(defmacro ifn (test ifnot &rest ifyes)
`(if (not ,test) ,ifnot ,@ifyes))

(defmacro end-push (what where)
  `(if ,where (push ,what (cdr (last ,where)))
      (push ,what ,where)))
(defun select (from-where match-test)
  "select items matching the test"
    (let (collected wasted)
       (dolist (list-item from-where)
	 (if (funcall match-test list-item)
	   (push list-item collected)
	   (push list-item wasted)))
(cons (reverse collected) (reverse wasted))))

;; -*- mode: Emacs-Lisp;  lexical-binding: t; -*-
(defun email (addr &optional subject body)
  "fast non-interactive way to send an email"
  (compose-mail addr (if subject subject ""))
  (when body (insert body))
  (message-send-and-exit))

(defun pos (el ll)
  (let ((i 0) r)
  (dolist (e ll r)
    (if (eql e el)
	(setf r i)
      (incf i)))))

(defun perms-from-str (str)
"parses file mode string into integer"
  (let ((text-mode (reverse (cdr (append str nil)))) (mode 0) (fac 1))
    (loop for c in text-mode for i from 0
          unless (= c ?-) do (incf mode fac)
          do (setf fac (* 2 fac)))
    mode))

(defun perms-to-str(file-mode)
"formats integer file mode into string"
(let ((ll '((1 . 0))))
  (apply #'concat (mapcar
		   #'(lambda(x) (format "%c" (if (= 0 (logand file-mode (car x))) ?- (aref "xwr" (cdr x)))))
  (dotimes (i 8 ll)
     (push (cons (* 2 (caar ll)) (mod (1+ i) 3))  ll))))))

(defun parse-date (str)
  (mapcar 'string-to-number
	  (cond
 ((string-match "\\([0-9]\\{4\\}\\)[/-]\\([0-9][0-9]\\)[/-]\\([0-9][0-9]\\)" str) (mapcar #'(lambda (x) (match-string x str)) '(3 2 1)))
 ((string-match "\\([0-9][0-9]\\)[/-]\\([0-9][0-9]\\)[/-]\\([0-9]\\{4\\}\\)" str) (mapcar #'(lambda (x) (match-string x str)) '(2 1 3)))
 ((string-match "\\([0-9][0-9]\\)\\.\\([0-9][0-9]\\)\\.\\([0-9]\\{4\\}\\)" str) (mapcar #'(lambda (x) (match-string x str)) '(1 2 3)))
 ((string-match "\\([0-9][0-9]\\)/\\([0-9][0-9]\\)/\\([0-9]\\{2\\}\\)" str) (mapcar #'(lambda (x) (match-string x str)) '(2 1 3)))
 ((string-match "\\([0-9]\\{2\\}\\)[/-]\\([0-9][0-9]\\)" str) (append (mapcar #'(lambda (x) (match-string x str)) '(2 1)) (list (format-time-string "%Y" (current-time)))))
 (t (clog :error "date format not recognized in %s" str) nil))))

(defun parse-only-time (str)
  (firstN (parse-time-string str) 3))

(defun parse-date-time(str)
  (if (string-match "[0-9]\\{4\\}-[0-9][0-9]-[0-9][0-9] [0-9][0-9]:[0-9][0-9]" str)
      (parse-time-string str)
    (let ((SS (split-string str)))
      (append (parse-only-time (cadr SS))
	      (parse-date (car SS))))))

(defun firstN(lista N)
  "returning first N elments of the list"
  (when (and (< 0 N) (car lista))
    (cons (car lista) (firstN (cdr lista) (1- N)))))

(defvar *good-chars*
(let ((forbidden-symbols '(?! ?@ ?# ?$ ?% ?& ?* ?\( ?\) ?+ ?= ?/ ?{ ?} ?\[ ?\] ?: ?\; ?< ?> ?_ ?- ?| ?, ?. ?` ?' ?~ ?^ ?\")))
    (append
     (loop for i from ?A to ?Z unless (member i forbidden-symbols) collect i)
     (loop for i from ?a to ?z unless (member i forbidden-symbols) collect i)
     (loop for i from ?0 to ?9 unless (member i forbidden-symbols) collect i)))
"safe characters for file names")
(defun rand-str(N)
  (apply #'concat
     (loop repeat N collect (string (nth (random (length *good-chars*)) *good-chars*)))))

(defun land(args)
"'and' for a list"
  (reduce #'(lambda(x y) (and x y)) args :initial-value t))
(defun safe-mkdir (dirname)
"creates a directory returning the report"
(condition-case err
  (progn (make-directory dirname)  (list t))
 (file-already-exists (cons nil :exists))
 (file-error (cons nil :permission))))

;; -*- mode: Emacs-Lisp;  lexical-binding: t; -*-
;; generated from https://notabug.org/shalaev/lisp-goodies/src/master/shalaev.org
(defun chgrp(group file-name)
  (= 0 (call-process "chgrp" nil nil nil group file-name)))

(defun rm(FN)
"erases files only, not directories"
  (condition-case err (cons t (delete-file FN))
    (file-error (cons nil (error-message-string err)))))

(defun safe-delete-dir (FN &optional recursive)
  (condition-case err (progn (delete-directory FN recursive) (list t))
    (file-error (cons nil (error-message-string err)))))
;; -*- mode: Emacs-Lisp;  lexical-binding: t; -*-

(defun cadar (x) (car (cdar x)))

;; specifying Moscow time zone (do this for other important unspecified time zones)
(unless (assoc "MSK" timezone-world-timezones)
  (push '("MSK". 300) timezone-world-timezones))
(unless (assoc "MSD" timezone-world-timezones)
  (push '("MSD". 400) timezone-world-timezones))

(defun parse-time(str)
  (let* ((TPD (timezone-parse-date str)) (TZ (aref TPD 4)))
    (unless (assoc TZ timezone-world-timezones)
      (clog :error "Unknown time zone abbreviation %s, update 'timezone-world-timezones' variable" TZ))
    (apply #'encode-time (append
			  (firstN
			   (parse-time-string
			    (format "%s-%s-%s %s" (aref TPD 0) (aref TPD 1) (aref TPD 2) (aref TPD 3)))
			   8)
			  (list (* 60 (timezone-zone-to-minute TZ)))))))

(defun full-TS (time)
  (format-time-string "%F %H:%M:%S %Z" time))

(defun TS (time)
  (format-time-string "%02m/%02d %H:%M:%S" time))

;; -*- mode: Emacs-Lisp;  lexical-binding: t; -*-

;; I try to get rid of loop and other common-lisp stuff here

;;(setf coding-system-for-read 'utf-8)
(defun safe-insert-file(FN)
  (let (failed)
  (condition-case err (insert-file-contents FN)
    (file-missing
     (clog :error "missing file %s: %s" FN (error-message-string err))
     (setf failed t))
    (file-error
     (clog :info "cannot read file %s; %s" FN (error-message-string err))
     (setf failed t)))
  (not failed)))

(defmacro temp-open(FN &rest body)
  `(with-temp-buffer
     (safe-insert-file ,FN)
     ,@body))

(defun backspace()
  (if (< (point-min) (point))
      (delete-char -1)
    (clog :error "can not backspace in buffer(%s), file(%s)"
	  (buffer-name)
	  (if-let ((FN (buffer-file-name))) FN "N/A"))))

(defun new-file-name (cloud-dir)
  (let (new-fname error-exists); экзистенциальная ошибка: какое бы имя я не выдумывал, а такой файл уже существует!
    (loop repeat 10 do (setf new-fname (rand-str 3))
          while (setf error-exists (file-exists-p (concat cloud-dir new-fname))))
    (if error-exists nil new-fname)))

(defun begins-with* (str what)
  (let ((ok t))
  (needs ((pattern
	   (case what; [\s-\|$] matches space or EOL
	     (:time "\s*\"\\([^\"]+\\)\"[\s-\|$]")
	     (:int "\s+\\([[:digit:]]+\\)[\s-\|$]")
	     (:string "[\s-]*\"\\(.+?\\)\"")
	     (:other "\s+\\([^\s]+\\)[\s-\|$]"))
	   (clog :error "invalid type %s in begins-with" what))
	  (MB (when (string-match pattern str) (match-beginning 1)))
	  (ME (match-end 1))
	  (matched (match-string 1 str)))
	 (case what
	   (:time
	      (let ((PTS (parse-time-string matched)))
		(if (setf ok (car PTS))
		  (cons
			(apply #'encode-time PTS)
			(substring-no-properties str (1+ ME)))
		  (clog :error "can not parse date/time string: %s"))))
	   (:int (cons
		      (string-to-number matched)
		      (substring-no-properties str ME)))
	   (:string
	    (cons (string-trim matched)
		  (substring-no-properties str (1+ ME))))
	   (:other
	    (cons (string-trim matched)
		  (substring-no-properties str ME)))))))

(defun begins-with (str what)
  (cond
   ((consp what)
    (let (result (ok t))
	(dotimes (i (cdr what))
	  (needs (ok (BW (begins-with str (car what)) (bad-column "N/A (begins-with)" i)))
		 (push (car BW) result)
		 (setf str (cdr BW))))
	(cons (reverse result) str)))
   ((eql :time-stamp what)
    (let ((res (begins-with* str :string)))
;;    (cons (apply #'encode-time (parse-time-string (car res))) (cdr res))))
    (cons (apply #'encode-time (parse-date-time (car res))) (cdr res))))
   ((eql :strings what)
    (let (BW result)
      (while (setf BW (begins-with* str :string))
	(push (car BW) result)
	(setf str (cdr BW)))
      (cons (reverse result) str)))
   (t (begins-with* str what))))

;; (defun old-cloud-locate-FN (FN)
;;   "find file by (true) name"
;;   (find FN file-DB :key #'plain-name
;; 	:test #'(lambda(x y)(string= (tilda x) (tilda y)))))

(defun cloud-locate-FN (FN)
  "find file by (true) name"
  (find (file-chase-links FN) file-DB :key #'plain-name
	:test #'(lambda(x y)(string= (tilda x) (tilda y)))))

(defun cloud-locate-CN (name)
  "find file by (ciper) name"
  (find name file-DB :key #'cipher-name :test #'string=))

 ;; Note sure if the following 2 functions are necessary, or, may be, they should be declared as macro or "inline":
(defun plain-name  (df)(aref df plain))
(defun cipher-name (df)(aref df cipher))

(defun DBrec-from-file(file-name)
  (when-let ((FA (file-attributes file-name 'string)))
    (let ((DBrec (make-vector (length file-fields) nil)))
      (destructuring-bind
	  (uid gid acess-time mod-time status-time fsize ms void inode fsNum)
	  (cddr FA)
	(aset DBrec-from-file size fsize)
	(aset DBrec-from-file gname gid)
	(aset DBrec-from-file mtime mod-time); list of 4 integers
	(aset DBrec-from-file modes (perms-from-str ms))
	(aset DBrec-from-file plain file-name))
      DBrec)))

;; grab-parameter is no more used as of 2020-09-18
;; (defun grab-parameter (str parname); (grab-parameter "contentsName=z12"  "contentsName") => "z12"
;;   (when (string-match (concat parname "=\\(\\ca+\\)$") str)
;;       (match-string 1 str)))
;; -*- mode: Emacs-Lisp;  lexical-binding: t; -*-
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
	(aset DB-rec plain FN); (aset DB-rec write-me to-cloud); might be later adjusted in read-fileDB
	DB-rec)))

(defun get-file-properties (FN)
  (or (cloud-locate-FN FN) (get-file-properties* (file-chase-links FN))))

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
(let ((tmp-gpg (concat /tmp/cloud/ XYZ ".gpg")))
(ifn (= 0 (apply #'call-process
(append (list "gpg" nil nil nil)
(split-string (format "--batch --yes --pinentry-mode loopback --passphrase %S -o %s --symmetric %s"
    password tmp-gpg (untilda FN))))))
(setf ok (clog :error "failed to encrypt %s to %s!" (local/all) remote/files))
(rename-file tmp-gpg (concat (remote-directory) XYZ ".gpg") t) t)))

(defun gpg-decrypt(FN XYZ)
(let ((tmp-gpg (concat /tmp/cloud/ XYZ ".gpg")))
  (copy-file (concat (remote-directory) XYZ ".gpg") tmp-gpg t)
  (ifn (= 0 (apply #'call-process
(append (list "gpg" nil nil nil)
(split-string (format "--batch --yes --pinentry-mode loopback --passphrase %S -o %s --decrypt %s"
		      password (untilda FN) tmp-gpg)))))
       (clog :error "failed to encrypt %s to %s!" (local/all) remote/files)
(rm tmp-gpg) t)))

(let ((~ (getenv "HOME")))
  (defun tilda(x)
    (replace-regexp-in-string (concat "^" ~) "~" x))
  (defun untilda(x)
    (replace-regexp-in-string "^~" ~ x)))

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
;; -*- mode: Emacs-Lisp;  lexical-binding: t; -*-
;; generated from cloud.org
(define-vars (password; to be read from config or generated
(number-of-CPU-cores 1)
cloud-file-hooks; "for special files treatment"

upload-queue; "names of edited files"
added-files; "newly clouded files"

remote/files; "3-symbol DB name on the server, e.g., WzT"
(localhost (system-name))
(~ (file-name-as-directory(expand-file-name "~")))
(remote-directory  "/mnt/cloud/")

(/tmp/cloud/ (file-name-as-directory (make-temp-file "cloud." t)))))
(defun remote/files() remote/files)
(defun remote-directory() remote-directory)
(defun remote-files() (concat (remote-directory) remote/files ".gpg"))
(defun history() (concat (remote-directory) "history"))

(defvar emacs-d "~/.emacs.d/")
(define-vars ((cloud-was-connected t))); normally t, nill when there was no connection

(defun local/config() (concat (local-dir) (file-name-as-directory localhost) "config"))

(define-vars ((numerical-parameters '("number-of-CPU-cores"))
 (lists-of-strings '("junk-extensions" "ignored-dirs"))))

(define-vars (cloud-hosts; host names participating in file synchronization
remote-actions; actions to be saved in the cloud
file-DB; list of vectors, each corresponding to a clouded file

file-blacklist
(ignored-dirs '("/tmp/" "/mnt/" "/etc/" "/ssh:")); temporary or system or remote directories

(junk-extensions '("ac3" "afm" "aux" "idx" "ilg" "ind" "avi" "bak" "bbl" "blg" "brf" "bst" "bz2" "cache" "chm" "cp" "cps" "dat" "deb" "dvi" "dv" "eps" "fb2"
"fn" "fls" "img" "iso" "gpx" "segments" "ky" "mjpeg" "m" "md" "mov" "mpg" "mkv" "jpg" "gif" "jpeg" "png" "log" "mp3" "mp4" "m2v" "ogg" "ogm" "out" "part" "pbm" "pdf"
"pfb" "pg" "pod" "pgm" "pnm" "ps" "rar" "raw" "gz" "sfd" "woff" "tbz" "tgz" "tga" "tif" "tiff" "toc" "tp" "vob" "vr" "wav" "xcf" "xml" "xz" "Z" "zip"))

(file-fields; indices numerating array fields
(list 'plain; original (local) file name
'cipher; encrypted file name (base name)
'mtime; modification time
'modes; permissions
'size; file size (should not be saved)
'gname)))); group name
(let ((i 0)) (dolist (field-name file-fields) (setf i (1+ (set field-name i)))))

(defun local/all() (concat (local/) "all"))

(defvar removed-files  nil "files that were just removed (or renamed or forgotten) on local host before (cloud-sync)")

(defvar important-msgs nil "these messages will be typically printed at the end of the process")
(defvar gpg-process nil "assyncronous make-process for (en/de)cryption")

(defvar action-fields '(i-time i-ID i-args i-hostnames i-Nargs))
(let ((i 0)) (dolist (AF action-fields) (setf i (1+ (set AF i)))))

(defvar action-IDs '(i-forget i-delete i-rename i-host-add i-host-forget i-share))
(let ((i 0)) (dolist (AI action-IDs) (setf i (1+ (set AI i)))))

(unless (boundp 'DRF) (defvar DRF (indirect-function (symbol-function 'dired-rename-file)) "original dired-rename-file function"))
(unless (boundp 'DDF) (defvar DDF (indirect-function (symbol-function 'dired-delete-file)) "original dired-delete-file function"))
(defun local-dir() (concat emacs-d (file-name-as-directory "cloud")))
(defun cloud-mk() (concat (local-dir) "cloud.mk"))
(defun lock-dir() (concat (remote-directory) (file-name-as-directory "now-syncing")))
(defun image-passes() (concat (local-dir) "individual.passes"))
(defun local/() (concat (local-dir) (file-name-as-directory localhost)))
(defun local/log() (concat (local/) "log"))

(defun cloud-init() "initializes cloud directory and generates password -- runs only once"
(clog :info "atempting to create new configuration for this host")
;;(when (yes-or-no-p "Is cloud mounted?")
;;(setf remote-directory (read-string "cloud directory=" remote-directory))
(ifn (member (safe-mkdir remote-directory) '(:exists t))
(clog :error "could not create/access directory %s" remote-directory)

(if (directory-files remote-directory nil "^.\+.gpg$" t)
    (clog :error "please clean the directory %s before asking me to initialize it" remote-directory)
(clog :info "creating (main) remote file DB in unused directory %s" remote-directory)
(ifn-set ((remote/files (new-file-name remote-directory)))
  (clog :error "could not create DB file in the directory %s" remote-directory)

(setf password (rand-str 9))

(ifn (member (safe-mkdir (local-dir)) '(:exists t))
  (clog :error "could not create/acess directory %s" (local-dir))
(write-conf)
(clog :info "use M-x cloud-add in the dired to cloud important files and directories" ))))))

(defun format-conf(CP)
(cond
  ((member CP numerical-parameters) (format "%s=%d" CP (symbol-value(intern CP))))
  ((member CP lists-of-strings) (format "%s=%s" CP
(apply #'concat (mapcar #'(lambda(item) (format "%s " item)) (sort (symbol-value(intern CP)) #'string<)))))
  (t (format "%s=%s" CP (symbol-value(intern CP))))))

(defun write-conf()
(clog :debug "starting write-conf")
(with-temp-file (local/config)
(mapcar #'(lambda(CP) (insert(format-conf CP)) (newline)) 
  '("remote-directory" "junk-extensions" "ignored-dirs" "remote/files" "number-of-CPU-cores" "password")))
(clog :debug "ended write-conf") t)

(defun read-conf* (file-name)
  "reads configuration file"
(with-temp-buffer
(safe-insert-file (local/config))
  (let (res str)
    (while (< 0 (length (setf str (read-line))))
     (if (string-match "^\\(\\ca+\\)=\\(\\ca+\\)$" str)
	 (push (cons (match-string 1 str) (match-string 2 str)) res)
       (clog :error "garbage string in configuration file: %s" str)))
    res)))

(defun read-conf()
  "reads configuration file"
(let ((conf (read-conf* (local/config))))
(ifn conf (clog :error "refuse to work until you specify 3-symbol contents name \"remote/files\" in %s" (local/config))
(dolist (CP (mapcar #'car conf))
(clog :debug "read-conf(%s)" CP)
  (setcdr (assoc CP conf)
    (cond
((member CP numerical-parameters) (string-to-number (cdr (assoc CP conf))))
((member CP lists-of-strings)  (split-string (cdr (assoc CP conf))))
(t (car (split-string (cdr (assoc CP conf))))))))
(clog :debug "done with read-conf")
conf)))
;; 2020-11-20 (car (split-string "/mnt/cloud/"))

(defun print-hosts()
  (dolist (hostname cloud-hosts) (insert (format "%s " hostname)))
  (backspace)
  (newline))

(defun print-actions()
(dolist (action remote-actions)
  (clog :debug "printing-action %s" (format-action action))
  (insert (format-action action))
  (drop remote-actions action)
  ;;(backspace) 
(newline)))

(defun format-file (DB-rec)
  (format "%S %s %s %s %d %S"
	  (tilda (aref DB-rec plain))
	  (aref DB-rec cipher)
	  (aref DB-rec size)
	  (aref DB-rec gname)
	  (aref DB-rec modes); integer
	  (format-time-string "%F %H:%M:%S %Z" (aref DB-rec mtime))))

(defun safe-FL()
  (if (< (line-end-position) (point-max))
     (forward-line)
     (move-end-of-line 1)))
(defun read-line()
(let ((str (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
 (safe-FL)
 str))
(defun cut-line() 
(let ((str (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
  (delete-region (line-beginning-position) (progn (safe-FL) (point)))
   str))

(defun parse-action(str)
(clog :debug "parse-action(%s) ..." str)
(let ((action (make-vector (length action-fields) nil)))

(dolist (column (list
                 `(:time-stamp . ,i-time)
                 `(:int . ,i-ID)
                 `(:int . ,i-Nargs)))
  (needs ((col-value (begins-with str (car column)) (bad-column "action" (cdr column))))
     (aset action (cdr column) (car col-value))
     (setf str (cdr col-value))))

(dolist (column 
(list
  (cons (cons  :string  (aref action i-Nargs)) i-args)
       `(:strings . ,i-hostnames)))
  (needs ((col-value (begins-with str (car column)) (bad-column "action" (cdr column))))
     (aset action (cdr column) (car col-value)); was (mapcar #'untilda (car col-value))
     (setf str (cdr col-value))))

(let ((AID (format-time-string "%02m/%02d %H:%M:%S" (aref action i-time))))
(clog :info "... will later be referenced as %s" AID)
(cons AID action))))

(defun str-to-DBrec(str)
"parses one file line from the remote file DB"
(ifn (string-match "\"\\(.+\\)\"\s+\\([^\s]+\\)\s+\\([^\s]+\\)\s+\\([^\s]+\\)\s+\\([[:digit:]]+\\)\s+\"\\(.+\\)\"" str)
(clog :error "Ignoring invalid file line %s" str)

(let ((CF (make-vector (length file-fields) nil))
      (FN (match-string 1 str)))
  (aset CF plain FN)
  (aset CF cipher (match-string 2 str))
  (aset CF size (string-to-number (match-string 3 str)))

(aset CF gname (match-string 4 str))
  (aset CF modes (string-to-number (match-string 5 str)))
  (let ((mtime-str (match-string 6 str)))
(ifn (string-match "[0-9]\\{4\\}-[0-9][0-9]-[0-9][0-9] [0-9][0-9]:[0-9][0-9]:[0-9][0-9] [[:upper:]]\\{3\\}" mtime-str)
(bad-column "file" 6 mtime-str)
(aset CF mtime (parse-time mtime-str))
CF)))))

(defun end-log (fstr &rest args)
  "message + time"
  (push
   (apply #'format
	  (cons (concat
		 (format-time-string "%H:%M:%S " (apply 'encode-time (butlast (decode-time (current-time)) 3)))
		 fstr)
		args))
   important-msgs))

(defun post-decrypt (FN)
  "special treatment for certain files"
  (let ((ext (file-name-extension FN))
	(name (file-name-base FN)))
    (when (string= FN (expand-file-name diary-file))
      (with-current-buffer (find-file-noselect (diary-check-diary-file))
	(clog :info "diary buffer opened or updated")))
     (when (member FN *loaded*)
       (end-log "*configuration changed, consider reloading emacs*")
    (clog :warning "consider reloading configuration file %s" FN)
    ;;   (load-file FN))
)))

(defun cloud-connected-p()
  (and
   (remote-directory) (remote/files)
   (file-readable-p remote-directory)))
;;(file-readable-p (remote-files)

(defun write-all (DBname)
  (with-temp-file DBname
(print-hosts)

(print-actions)

(newline)

(dolist (file-record file-DB)
  (insert (format-file file-record)) (newline))
(setf removed-files nil) t))

(defun read-all(DBname)
  "reads content (text) file into the database file-DB"
  (temp-open DBname
  (let (str)
(needs-set
 ((cloud-hosts
  (split-string (setf str (read-line)))
  (clog :error "invalid first line in the remote file DB %s" DBname)))

(unless (member localhost cloud-hosts) (cloud-host-add))

(while (< 0 (length (setf str (read-line))))
(clog :debug "action string=%s" str)
(when-let ((AA (parse-action str)) (AID (car AA)) (action (cdr AA)))
  (ifn (member localhost (aref action i-hostnames))
      (clog :info "this host is unaffected by action %s" AID)
    (if (perform action (aref action i-hostnames))
	(clog :info "sucessfully performed action %s" AID)
      (clog :error " action %s failed, will NOT retry it" AID))

(when (drop (aref action i-hostnames) localhost)
  (end-push action remote-actions)))))

(needs ((CDFs

(mapcar #'(lambda(s) (replace-regexp-in-string "....$" "" s))
      (directory-files remote-directory nil "...\...." t)) (clog :error "can not read %s" remote-directory)))
(while(< 10 (length (setf str (read-line))))
(when-let((CF (str-to-DBrec str)))

(let* ((FN (plain-name CF))
       (CN (aref CF cipher))
       (remote-file-exists (member CN CDFs))
       (local-file-rec (or (cloud-locate-FN FN)
(and (not (member FN removed-files))
(when-let ((LF (get-file-properties* FN)))
        (aset LF cipher (aref CF cipher))
        (push LF file-DB)
        LF)))))
(cond
((not (or local-file-rec remote-file-exists))
 (clog :error "forgetting file %s which is marked as clouded but is neither on local disk nor in the cloud" FN)
 (drop file-DB CF))
((or
 (and (not local-file-rec) remote-file-exists)
 (and local-file-rec remote-file-exists (time< (aref local-file-rec mtime) (aref CF mtime))))
(when (and local-file-rec remote-file-exists)
  (clog :debug "read-all/download: %s(%s) is older than %s.gpg(%s)"
  (aref local-file-rec plain) (TS(aref local-file-rec mtime))
  (aref CF cipher) (TS(aref CF mtime))))

(if local-file-rec
   (aset local-file-rec mtime (aref CF mtime))
   (push CF file-DB))
(let*((DN(file-name-directory(aref CF plain))) (mkdir(safe-mkdir DN)))
(if(or(car mkdir)(eql :exists(cdr mkdir)))
(download CF)
(clog :error "could not mkdir %s" DN))))
((or
 (and local-file-rec remote-file-exists (time< (aref CF mtime) (aref local-file-rec mtime)))
 (and local-file-rec (not remote-file-exists)))
(when (and local-file-rec remote-file-exists)
  (clog :debug "read-all/upload: local %s(%s) is younger than %s.gpg(%s)"
  (aref local-file-rec plain) (TS(aref local-file-rec mtime))
  (aref CF cipher) (TS(aref CF mtime))))
(upload CF))))))
t)))))

(defun touch (FN)
"called when the file named FN is changed"
(when (and FN (stringp FN))
  (when-let ((file-data (cloud-locate-FN FN)))
    (aset file-data mtime (current-time))
    (clog :debug "touch/upload: %s(%s)" FN (TS(aref file-data mtime)))
    (upload file-data) t)))
(defun on-current-buffer-save()
  (when-let ((FN (buffer-file-name)))
    (and (auto-add-file FN) (touch FN))))
(add-hook 'after-save-hook 'on-current-buffer-save)

(defmacro NL () '(push "
" Makefile))
(defmacro inl (&rest format-pars) `(progn (push ,(cons 'format format-pars) Makefile) (NL)))
(define-vars (all Makefile uploaded))

(defun cancel-pending-upload(FN) (drop stanze FN))
(defun pass-d () (concat (local-dir) (file-name-as-directory "pass.d")))
(defun updated() (concat (pass-d) "updated"))

(defun enc-make-stanza(file-record)
  (when-let ((XYZ (aref file-record cipher)) (FN (tilda (aref file-record plain))))

(let ((file-ext (file-name-extension FN)))
(concat (cond

((string= "gz" file-ext)
(let ((gunzipped (make-temp-file "emacs-cloud.")))
(format "
%s: %s
\tzcat $< > $@

$(cloud)%s.gpg: %s
\t$(enc) $@ $<
\trm $<
" gunzipped FN XYZ gunzipped)))

((string= "gpg" file-ext)
(format "
$(cloud)%s.gpg: %s
\tcp $< $@
" XYZ FN))

((member file-ext '("jpg" "jpeg" "png"))
(format "
$(cloud)%s.png: %s %s
\tconvert $< -encipher %s%s $@
"
XYZ FN (updated)
(pass-d) XYZ))

(t (format "
$(cloud)%s.gpg: %s
\t$(enc) $@ $<
" XYZ FN)))

"\t-@echo \"$$(date): uploaded $<\" >> $(localLog)
"))))

(defun dec-make-stanza(file-record)
  (when-let ((XYZ (aref file-record cipher)) (FN (tilda (aref file-record plain))))
    (let ((file-ext (file-name-extension FN)))
(concat
(cond

((string= "gpg" file-ext)
(format "
%s: $(cloud)%s.gpg
\tcp $< $@
" FN XYZ))

((member file-ext '("jpg" "jpeg" "png"))
(format "
%s: $(cloud)%s.png  %s
\tconvert $< -decipher %s%s $@
"
FN XYZ (updated)
(pass-d) XYZ))

((string= "gz" file-ext)
(let ((gunzipped (make-temp-file "emacs-cloud.")))
  (format "
%s:$(cloud)%s.gpg
\t$(dec) $@ $<

%s: %s
\tcat $< | gzip > $@
\trm $<
" 
gunzipped XYZ
FN gunzipped)))

(t (format "
%s: $(cloud)%s.gpg
\t$(dec) $@ $<
" FN XYZ)))

(format "\t-chgrp %s $@
\t-chmod %o $@
\t-touch --date=%S $@
\t-@echo \"$$(date): downloaded $@\" >> $(localLog)

"
(aref file-record gname) (aref file-record modes) (full-TS (aref file-record mtime)))))))

(defun download(file-record)
(needs ((FN (aref file-record plain) (clog :error "download: file lacks plain name"))
        (stanza (dec-make-stanza file-record) (clog :error "download: could not create stanza for %s" FN)))
(safe-mkdir (file-name-directory FN))
(push (format " %s" FN) stanze)
(push stanza Makefile) (NL)))

(defun make-cloud-older(file-record)
(when-let ((clouded (get-file-properties (aref file-record cipher)))
           (local-mtime (aref file-record mtime)))
(when (time< local-mtime (aref clouded mtime))
(clog :debug "changing time stamp to %s" (FS (time-add local-mtime -60)))
  (set-file-times
(concat (remote-directory) (plain-name clouded) (cip-ext (plain-name file-record)))
(time-add local-mtime (- -60 (random 6000)))))))

(defun upload (file-record)
(needs ((FN (tilda (aref file-record plain)) (clog :error "upload: file lacks plain name"))
	(CN (aref file-record cipher) (clog :error "upload: file %s lacks cipher name" FN))
	(stanza (enc-make-stanza file-record) (clog :error "upload: could not create stanza for %s" FN)))
(clog :debug "started upload(%s)" FN)
(unless (or (member FN uploaded) (member FN file-blacklist))
(push FN upload-queue)
(clog :debug "will add upload(%s) stanza to Makefile" FN)
(make-cloud-older file-record)
(push FN uploaded)
(push (format " %s" (concat (remote-directory) CN
(cip-ext FN)))
stanze)
(push stanza Makefile) (NL))))

(defun reset-Makefile()
"reseting make file"
(when (or (and (file-exists-p (pass-d)) (file-directory-p (pass-d))) (safe-mkdir (pass-d)))
(setf stanze nil Makefile nil uploaded nil)
(inl "cloud=%s" remote-directory)
(inl "password=%S" password)
(inl "gpg=gpg --pinentry-mode loopback --batch --yes")
(inl "enc=$(gpg) --symmetric --passphrase $(password) -o")
(inl "dec=$(gpg) --decrypt   --passphrase $(password) -o")
(inl "localLog=%s" (local/log))
(inl "MK=%s" (cloud-mk))
(inl "date=`date '+%%m/%%d %%T'`
")
(inl (concat (format "%s: %s
\tawk '{print $$2 > %S$$1}' $<
\techo $(date) > $@
\t-chgrp -R tmp %s*
" (updated) (image-passes) (untilda (pass-d)) (pass-d))))))

(defun save-Makefile()
"flushing make file"
(inl "all:%s
\techo \"background (en/de)cryption on %s finished $(date)\" >> %s
\t@sed 's/%s/******/g' %s > %s.bak
"
(apply #'concat stanze)
localhost
(history)
password (cloud-mk) (cloud-mk))
(write-region (apply #'concat (reverse Makefile)) nil (cloud-mk)))

(defun cloud-sync()
(interactive)
(let ((ok t))

(ifn (cloud-connected-p) (clog :warning "refuse to sync because remote directory not mounted")

(let ((DL (directory-lock (lock-dir) (format "%s
%s" localhost (TS (current-time)))

(when (file-newer-than-file-p (remote-files) (local/all))
  (clog :info "detected NEW %s, will now update %s from it" (remote-files) (local/all))
  (unless (gpg-decrypt (local/all) (remote/files))
    (setf ok (clog :error "could not decrypt file data from the cloud; SHUT DOWN the service and INVESTIGATE!"))))

(unless (read-all (local/all))
 (setf ok (clog :error "could not parse file data just downloaded from the cloud; SHUT DOWN the service and INVESTIGATE!")))

(when (or added-files upload-queue removed-files)
  (ifn (write-all (local/all)) (setf ok (clog :error "could not save data to %s" (local/all)))
    (gpg-encrypt (local/all) (remote/files))
    (setf added-files nil upload-queue nil removed-files nil)))

(set-file-times (local/all) (current-time))

(save-Makefile)
(let ((make (format "make -j%d -f %s all &> %s.log" number-of-CPU-cores (cloud-mk) (cloud-mk))))
  (clog :debug "starting %s" make)
  (shell-command make)
  (clog :debug "finished %s" make))
(rm (cloud-mk))
(reset-Makefile))))

(unless (car DL) (setf ok (clog :error "Could not (un)lock remote directory! Please investigate"))))

(dolist (msg (reverse important-msgs)) (message msg))
(setf important-msgs nil)
(clog :info "done syncing")
(write-region (format "%s: %s -- %s
" localhost  (TS (current-time)) (format-time-string "%H:%M:%S" (current-time))) nil (history) t))
ok))

(defun before-exit()
;; (write-conf)
(when (cloud-sync)
  (safe-delete-dir /tmp/cloud/)))

(defun new-action (a-ID &rest args)
(mapcar #'(lambda(FN) (clog :debug "new-action(%d %s)" a-ID FN)) args)
  (let ((action (make-vector (length action-fields) nil)))
    (aset action i-ID a-ID)
    (aset action i-time (current-time))
    (aset action i-args args)
    (aset action i-hostnames (remove localhost cloud-hosts))
    (end-push action remote-actions)))

(defun perform(action &optional HNs)
"performing an action locally"
(write-region
(format "%s: %s
" (TS (current-time)) (format-action action))
nil (local/log) t)
  (let ((arguments (aref action i-args)))
    (case* (aref action i-ID) =
      (i-host-forget (dolist (arg arguments) (drop cloud-hosts arg)) t)
      (i-host-add (dolist (arg arguments) (push arg cloud-hosts)) t)
      (i-forget (cloud-forget-many arguments) t)
      (i-delete (cloud-rm arguments) t)
      (i-rename (cloud-rename-file (first arguments) (second arguments)) t)

(i-share (when (= 1 (length HNs)) (cloud-forget-many arguments)))
(otherwise (clog :error "unknown action %d" (aref action i-ID))))) t)

(defun format-action (action)
  (format "%S %d %d %s %s"
(full-TS (aref action i-time)); 1. Time stamp,
(aref action i-ID); 2. (integer) action ID,
(length (aref action i-args)); 3. (integer) number of arguments for this action (one column),
(apply #'concat (mapcar #'(lambda(arg) (format "%S " (tilda arg))) (aref action i-args))); 4. [arguments+] (several columns),
(apply #'concat (mapcar #'(lambda(HN) (format "%S " HN)) (aref action i-hostnames))))); 5. hostnames, where the action has to be performed (several columns).

(defun dired-delete-file (FN &optional dirP TRASH)
  (let ((FN (tilda FN))); ~/programming/emacs/functions.el
(when (car    
       (condition-case err (cons t (funcall DDF FN dirP TRASH))
	 (file-error (clog :error "in DDF: %s" (error-message-string err)))))
  (cons t (and (cloud-forget-recursive FN)
	       (new-action i-delete FN))))))

(defun cloud-rm (args)
(let ((ok (cloud-forget-many args)))
  (dolist (arg args)
    (setf ok (and (safe-delete-dir arg t) (cloud-forget-recursive arg) ok)))
ok))

(defun cloud-forget-many (args)
  (interactive) 
(let ((ok t))
  (dolist (arg args)
    (setf ok (and (cloud-forget-recursive arg) ok)))
ok))

(defun contained-in(DN)
  (let* ((dir-name (tilda DN)) res (dir-name (file-name-as-directory dir-name)))
    (dolist (DB-rec file-DB)
      (when(and
(< (length dir-name) (length (aref DB-rec plain)))
(string=(substring-no-properties (aref DB-rec plain) 0 (length dir-name)) dir-name))
        (push DB-rec res)))
      res))

(defun add-to-actions(hostname)
  (dolist (action remote-actions)
    (unless (member hostname (aref action i-hostnames))
      (aset action i-hostnames (cons hostname (aref action i-hostnames))))))
(defun erase-from-actions(hostname)
  (dolist (action remote-actions)
    (when (member hostname (aref action i-hostnames))
      (aset action i-hostnames (remove hostname (aref action i-hostnames))))))

(defun cloud-host-add ()
  "adding THIS host to the cloud sync-system"
  (unless (member localhost cloud-hosts)
    (push localhost cloud-hosts))
  (new-action i-host-add localhost)
  (add-to-actions localhost))

(defun cloud-host-forget ()
  "remove host from the cloud sync-system"
    (when (yes-or-no-p (format "Forget the host %s?" localhost))
      (new-action i-host-forget localhost)
      (if (cloud-sync)
	  (safe-dired-delete (local/config))
	(clog :error "sync failed, so I will not erase local configuration"))))

(defun cloud-add(&optional FN)
(interactive)
(if FN (add-file FN)
  (if (string= major-mode "dired-mode")
      (dired-map-over-marks (add-file (dired-get-filename)) nil)
(if-let ((FN (buffer-file-name))) (add-file FN)
    (unless
	(add-file (read-string "file to be clouded=" (if FN FN "")))
      (clog :error "could not cloud this file"))))))

(defun blacklist(FN)
(let ((FN (tilda FN)))
 (cloud-forget-file FN)
(unless (member FN file-blacklist)
 (push FN file-blacklist))))
(defun black-p(FN &optional file-rec)
(let ((result
(or
 (member FN file-blacklist) (string-match "tmp" FN)
 (string-match (concat ~ "\\.") (untilda FN))
 (member (file-name-extension FN) junk-extensions)
 (backup-file-name-p FN)
 (when ignored-dirs (string-match(substring(apply #'concat
(mapcar #'(lambda(d)(format "\\(^%s\\)\\|" d)) ignored-dirs)) 0 -2) FN))
 (progn
   (unless file-rec (setf file-rec (get-file-properties FN)))
   (when file-rec
     (or
      (member (aref file-rec gname) '("tmp"))
      (< 1000000 (aref file-rec size))))))))
  (cons result file-rec)))

(defun white-p(FN &optional file-rec)
(unless file-rec (setf file-rec (get-file-properties FN)))
(cons (member (aref file-rec gname) '("important" "keepOneYear" "keepTwoYears" "keepThreeYears")) file-rec))

(defun add-file(FN &optional file-rec)
(let ((FN (tilda (file-chase-links FN))))
(unless (cloud-locate-FN FN)
(ifn (file-directory-p FN)
  (needs ((GFP (or file-rec (get-file-properties* FN)) (clog :error "Aborting attempt to cloud inexisting file %s" FN))
          (CN (new-file-name remote-directory)))
(push FN added-files)
    (aset GFP cipher CN)
    (push GFP file-DB)
    (clog :debug "add-file/upload: %s(%s)" FN (TS(aref GFP mtime)))
    (upload GFP)
    (when (member (file-name-extension FN) '("jpeg" "png" "jpg"))

(write-region
  (format "%s %s
" CN (rand-str 18)) nil (image-passes) t)
(touch (image-passes))))

(let ((DN (file-name-as-directory FN)))
(dolist (FN (directory-files DN nil nil t))
(unless (member FN '("." ".."))
(let ((FN (concat DN FN)) FR)

(if (or
(let ((r (white-p FN))) (setf FR (cdr r)) (car r))
(not
(let ((r (black-p FN FR))) (setf FR (cdr r)) (car r))))
(add-file FN FR)
(clog :debug "not auto-clouding %s" FN))))))))))

(defun auto-add-file(FN &optional file-rec)
"when the file is clouded automatically"
 (unless (car(black-p FN file-rec)) (add-file FN file-rec)) t)

(defun cloud-forget-file (FN)
(clog :debug "cloud-forget-file (%s)" FN)
  (needs ((DB-rec (cloud-locate-FN FN); or (old-cloud-locate-FN FN))
 (clog :warning "forget: doing nothing since %s is not clouded" FN))
          (CEXT (cip-ext FN))
	  (cloud-FN (concat (remote-directory) (aref DB-rec cipher) CEXT) (clog :error "in DB entry for %s" FN)))
(cancel-pending-upload FN)

(when (string= CEXT ".png")
(clog :debug "forgetting password for %s" FN)
  (forget-password (aref DB-rec cipher)))

(drop file-DB DB-rec)
(push FN removed-files)
(if (car (safe-dired-delete cloud-FN))
  (clog :info "erased %s" cloud-FN)
  (clog :warning "could not erase %s" cloud-FN))
 t))

(defun cloud-forget-recursive(FN)
(clog :debug "cloud-forget-recursive(%s)" FN)
(new-action i-forget FN)
(dolist (sub-FN (mapcar #'plain-name (contained-in FN)))
  (cloud-forget-file sub-FN))
(cloud-forget-file FN))

(defun cloud-forget (&optional FN)
  (interactive)
(if FN (cloud-forget-recursive FN)
  (if (string= major-mode "dired-mode")
      (dired-map-over-marks (cloud-forget-recursive (dired-get-filename)) nil)
(if-let ((FN (buffer-file-name))) (cloud-forget-recursive FN)
    (unless
	(cloud-forget-recursive (read-string "file to be forgotten=" (if FN FN "")))
      (clog :error "could not forget this file"))))))

(defun cloud-rename-file(old new)
  (let ((source (cloud-locate-FN old))
        (target (cloud-locate-FN new)))
    (cond
     ((and source target); overwriting one cloud file with another one
      (dolist (property (list mtime modes gname))
            (aset target property (aref source property)))
      (drop file-DB source))
     (source (aset source plain new))
     (target (setf target (get-file-properties* new))))))

(defun dired-rename-file (old-FN new-FN ok-if-already-exists)
  (let (failure)
    (clog :debug "DRF")
    (condition-case err
	(funcall DRF old-FN new-FN ok-if-already-exists)
      (file-error
       (clog :error "DRF error!")
       (message "%s" (error-message-string err))
       (setf failure t)))
    (unless failure
      (clog :debug "cloud-rename-file %s --> %s" old-FN new-FN)
      (cloud-rename-file old-FN new-FN)
      (new-action i-rename old-FN new-FN)

(when (file-directory-p old-FN)
  (let* ((old-dir (file-name-as-directory old-FN)) (LOD (length old-dir))
         (new-dir (file-name-as-directory new-FN)))
    (dolist (rec (contained-in old-FN))
      (let ((FN (aref rec plain)))
        (when (and (<= LOD (length FN))
	     (string= old-FN (substring FN 0 LOD)))
	  (let ((new-name (concat new-dir (substring FN LOD))))
            (cloud-rename-file FN new-name)
	    (new-action i-rename FN new-name))))))))))

(defun rename-directory (old-dir new-dir)
"recursively update plain-names of clouded files due to renaming of a directory"
(when (file-directory-p old-dir)
  (let* ((old-dir (file-name-as-directory old-dir)) (LOD (length old-dir))
         (new-dir (file-name-as-directory new-dir)))
    (dolist (rec (contained-in old-dir))
      (let ((FN (aref rec plain)))
        (when (and (<= LOD (length FN))
		   (string= old-dir (substring FN 0 LOD)))
	  (aset rec plain (concat new-dir (substring FN LOD)))))))))

(defun update-conf(conf &rest conf-params)
(clog :debug "started update-conf")
  (dolist (CP conf-params)
    (when-let ((CPV (cdr (assoc CP conf)))) (set (intern CP) CPV)))
(clog :debug "ended update-conf"))

(defun cloud-start()
(save-some-buffers)
(clog :debug "cloud-start: local/config = %s" (local/config))
(ifn-let ((conf (read-conf)))
(progn
  (clog :warning "could not read local configuration file, trying to (re)create configuration")
  (when (cloud-init)
  (clog :info "check newly created configuraion %s and then M-x cloud-start" (local/config))))

(update-conf conf "remote-directory" "junk-extensions" "ignored-dirs" "remote/files" "number-of-CPU-cores" "password")

(ifn remote-directory (clog :error "You have to set remote-directory for me before I can proceed")
(ifn password (clog :error "You have to set encryption password for me before I can proceed")

(add-hook 'kill-emacs-hook 'before-exit)

(unless (file-exists-p (image-passes))
  (write-region "" nil (image-passes))
  (add-file (image-passes)))

(reset-Makefile)
(cloud-sync)))))

(defun read-fileDB()
(clog :debug "starting read-fileDB")
(or
(and
;; (cloud-connected-p)
(= 0 (apply #'call-process
(append (list "gpg" nil nil nil)
(split-string (format
"--batch --yes --pinentry-mode loopback --passphrase %s -o %s --decrypt %s"

password (untilda (local/all)) (remote-files))))))
(read-all (local/all)))
(clog :error "cloud-start header failed") nil))
