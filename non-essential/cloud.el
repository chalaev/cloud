;; -*- lexical-binding: t -*-

;;;cloud.el --- secure cloud storage and syncronization for text files

;; Copyright (C) 2020 Oleg Shalaev <oleg@chalaev.com>

;; Author:     Oleg Shalaev <oleg@chalaev.com>
;; Version:    0.1
;; Package-Requires: (cl epg dired-aux timezone diary-lib subr-x)
;; Keywords:   syncronization, cloud, gpg, encryption
;; URL:        https://github.com/chalaev/cloud

;;; Commentary:

;; This package shares text files between several computers used by one person.
;; For quick start and documentation see
;; https://github.com/chalaev/cloud
;; https://github.com/chalaev/cloud/blob/master/cloud.org
  
;;; Code:
;;; -*- mode: Emacs-Lisp;  lexical-binding: t; -*-

(defun cadar (x) (car (cdar x)))

(unless (functionp 'gensym)
(let ((counter 0))
(clog :debug "macros/gensym: lexical-binding= %s" (if lexical-binding "true" "false"))
  (defun gensym(&optional starts-with)
    "for those who miss gensym from Common Lisp"
    (unless starts-with (setf starts-with "gs"))
    (let (sym)
      (while (progn
               (setf sym (make-symbol (concat starts-with (number-to-string counter))))
               (or (special-form-p sym) (functionp sym) (macrop sym) (boundp sym)))
        (incf counter))
      (incf counter)
      sym))))

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

(defun TS (time)
  (format-time-string "%02m/%02d %H:%M:%S" time))

;; test: (TS (parse-time "2020-10-10 14:54:40 MSK"))

;;(parse-time "2019-09-05 16:09:37 EDT")
;;; -*- mode: Emacs-Lisp;  lexical-binding: t; -*-

;; generated from https://github.com/chalaev/elisp-goodies/blob/master/goodies.org
(defmacro case= (var &rest cases)
  "case with integer equality (=) as a test function"
  (let ((v (gensym "v")))
    `(let ((,v ,var))
       (cond
        ,@(mapcar #'(lambda (VR)
(let ((val (car VR)) (rest (cdr VR)))
  (if (eql val 'otherwise)
      `(t ,@rest)
    `((= ,v ,val) ,@rest))))
 cases)))))

(defmacro when-let (vars &rest body)
  "when with let using stndard let-notation"
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

(defmacro when-set (vars &rest body)
  "when-let using global variable instead of defining local one"
  `(progn (setf ,(caar vars) ,(cadar vars)); get rid of progn here
     ,(if (cdr vars)
          `(when ,(caar vars)
             ,(macroexpand-1 `(when-set ,(cdr vars) ,@body)))
        (append `(when ,(caar vars)) body))))

(defmacro if-let (vars &rest body)
  "if with let using stndard let-notation"
  (let ((if-true (gensym "it")) (result (gensym "r")))
    `(let (,if-true ,result)
       (when-let ,vars
                 (setf ,if-true t)
                 (setf ,result ,(car body)))
       (if ,if-true
           ,result
         ,@(cdr body)))))

(defmacro ifn-let (vars &rest body)
  `(if-let ,vars
      ,(cons 'progn (cdr body))
    ,(car body)))

(defmacro ifn-set (vars &rest body)
  `(if-set ,vars
      ,(cons 'progn (cdr body))
    ,(car body)))

(defmacro if-set (vars &rest body)
  (let ((if-true (gensym "it")))
    `(let (,if-true)
       (when-set ,vars
                  (setf ,if-true t)
                  ,(car body))
       (unless ,if-true
         ,@(cdr body)))))

(defmacro cond-let (&rest conds)
  "cond with let"
  (let ((c (car conds)) (r (cdr conds)))
    (if (equal (car c) 'otherwise) (cons 'progn (cdr c))
    (if r
        `(if-let ,(car c) ,(cons 'progn (cdr c)) ,(macroexpand-1 `(cond-let ,@r)))
        `(when-let ,(car c) ,@(cdr c))))))

(defmacro needs (&rest all-args)
  "unifying when-let and if-let"
  (let* ((vardefs (car all-args))
        (body (cdr all-args))
        (vardef (car vardefs)))
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

(defmacro needs-set (&rest all-args)
  "needs with 'let' being replaced with 'setf'"
  (let* ((vardefs (car all-args))
        (body (cdr all-args))
        (vardef (car vardefs)))
    (if (and (listp vardef) (not (or (special-form-p (car vardef)) (functionp (car vardef)) (macrop (car vardef)))))
    `(progn (setf ,(car vardef) ,(cadr vardef))
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

(defmacro ifn (test ifnot &rest ifyes)
`(if (not ,test) ,ifnot ,@ifyes))
;;; -*- mode: Emacs-Lisp;  lexical-binding: t; -*-

;; generated from https://github.com/chalaev/elisp-goodies/blob/master/goodies.org
(defun chgrp(group file-name)
  (let (process (counter 300) (buffer (generate-new-buffer " *chgrp*")))
    (setf process (apply #'start-process "cloud-chgrp" buffer "chgrp" (list group file-name)))
    (while (and (> counter 0) (eq (process-status process) 'run))
      (decf counter) (sleep-for 0.1))))

(unless (functionp 'gensym)
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
      sym))))

(defun email (addr &optional subject body)
  "fast non-interactive way to send an email"
  (compose-mail addr (if subject subject ""))
  (when body (insert body))
  (message-send-and-exit))

(defun remo (from-where &rest what)
  (if (cdr what)
      (remo
       (apply #'remo (cons from-where (cdr what)))
       (car what))
 (remove (car what) from-where)))
(defmacro drop (from-where &rest what)
  `(setf ,from-where (remo ,from-where ,@what)))

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

(unless (or (boundp 'decf) (functionp 'decf) (macrop 'decf))
(defmacro decf (var &optional amount)
  (unless amount (setf amount 1))
  `(setf ,var (- ,var ,amount))))

(unless (or (boundp 'incf) (functionp 'incf) (macrop 'incf))
(defmacro incf (var &optional amount)
  (unless amount (setf amount 1))
  `(setf ,var (+ ,var ,amount))))

(defun pos (el ll)
  (let ((i 0) r)
  (dolist (e ll r)
    (if (eql e el)
        (setf r i)
      (incf i)))))
;;; -*- mode: Emacs-Lisp;  lexical-binding: t; -*-

;; generated from https://github.com/chalaev/elisp-goodies/blob/master/goodies.org
(unless (boundp '*log-level*) (defvar *log-level* 0))
(unless (boundp '*emacs-d*) (defvar *emacs-d* (concat (getenv "HOME") "/.emacs.d/")))

(unless (boundp '*file-acc-buffer*) (defvar *file-acc-buffer* nil))
(defvar *last-FLD* nil "saves last day printed to the log file")

(defun clog-flush()
  "save log messages to file for debugging"
  (when (= 0 *log-level*)
    (with-temp-buffer
      (let ((today-str (format-time-string "%04Y-%02m-%02d" (current-time))))
        (unless (string= today-str *last-FLD*)
          (setf *last-FLD* today-str)
          (insert today-str) (newline))
        (dolist (msg (reverse *file-acc-buffer*))
          (insert msg) (newline)))
      (append-to-file (point-min) (point-max) (concat *emacs-d* "elisp.log")))
    (setf *file-acc-buffer* nil)))

(defun file-acc-push(msg)
  (when (= 0 *log-level*)
    (push msg *file-acc-buffer*)
    (when (< 30 (length *file-acc-buffer*)) (clog-flush))))

(defun clog (level fstr &rest args)
  "simple logging function" ; level is one of → :debug :info :warning :error
  (when (<= *log-level* (or (pos level '(:debug :info :warning :error)) 0))
    (let ((log-msg (cons (concat "%s "
(format-time-string "%H:%M:%S "
                    (apply 'encode-time (butlast (decode-time (current-time)) 3)))
fstr)
(cons (symbol-name level) args))))
(file-acc-push (apply #'format log-msg))
(apply #'message log-msg))))

(defun on-emacs-exit()
  (clog :debug "flushing comments before quiting emacs")
  (clog-flush))

(add-hook 'kill-emacs-hook 'on-emacs-exit)
;;; -*- mode: Emacs-Lisp;  lexical-binding: t; -*-

;; I try to get rid of loop and other common-lisp stuff here

(defun firstN(lista N)
  "returning first N elments of the list"
  (when (and (< 0 N) (car lista))
    (cons (car lista) (firstN (cdr lista) (1- N)))))

(defvar *all-chars*
  (let ((forbidden-symbols '(?! ?@ ?# ?$ ?% ?& ?* ?( ?) ?+ ?= ?/ 
                      ?{ ?} ?[ ?] ?: ?\; ?< ?>
                      ?_ ?- ?| ?, ?. ?` ?' ?~ ?^ ?\")))
    (append
     (loop for i from ?A to ?Z unless (member i forbidden-symbols) collect i)
     (loop for i from ?a to ?z unless (member i forbidden-symbols) collect i)
     (loop for i from ?0 to ?9 unless (member i forbidden-symbols) collect i))))

(defun time< (t1 t2)
  (and
    (time-less-p (time-add t1 1) t2)
    (not (time-less-p (time-add t2 1) t1))))

(defun safe-mkdir (dirname)
  (if (file-exists-p dirname)
      (if (file-directory-p dirname)
          (progn (message "not creating already existing directory %s" dirname) :exists)
        (message "file exists with the same name as working directory %s" dirname) :file)
    (condition-case err
        (progn (make-directory dirname) t)
      (file-already-exists (message "Strange, may be the file already exists (but this was checked!). %s" (error-message-string err)) nil); when file exists
      (file-error (message "Probably, you have not permission to create this directory: %s" (error-message-string err)) :permission))))

(defun safe-dired-delete (FN)
  (let (failure)
    (condition-case err (funcall DDF FN "always")
      (file-error
       (clog :error "in DDF: %s" (error-message-string err))
       (setf failure t)))
    (not failure)))

(defun safe-delete-file (FN)
  (let (failed)
  (condition-case err (delete-file FN)
    (file-error
     (clog :info "cannot delete file %s; %s" FN (error-message-string err))
     (setf failed t)))
  (not failed)))

(defun safe-delete-dir (FN)
  (let (failed)
  (condition-case err (delete-directory FN)
    (file-error
     (clog :info "cannot delete directory %s; %s" FN (error-message-string err))
     (setf failed t)))
  (not failed)))

(defun rand-str (N)
  (apply #'concat
         (loop repeat N collect (string (nth (random (length *all-chars*)) *all-chars*)))))

(defun new-file-name (cloud-dir)
  (let (new-fname error-exists); экзистенциальная ошибка: какое бы имя я не выдумывал, а такой файл уже существует!
    (loop repeat 10 do (setf new-fname (rand-str 3))
          while (setf error-exists (file-exists-p (concat cloud-dir new-fname))))
    (if error-exists nil new-fname)))

(defun perms-from-str (str); e.g., (perms-from-str "-rw-rw----") => #o660
  (let ((text-mode (reverse (cdr (append str nil)))) (mode 0) (fac 1))
    (loop for c in text-mode for i from 0
          unless (= c ?-) do (incf mode fac)
          do (setf fac (* 2 fac)))
    mode))

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
		      (string-to-int matched)
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
    (cons (apply #'encode-time (parse-time-string (car res))) (cdr res))))
   ((eql :strings what)
    (let (BW result)
      (while (setf BW (begins-with* str :string))
	(push (car BW) result)
	(setf str (cdr BW)))
      (cons (reverse result) str)))
   (t (begins-with* str what))))

(defun cloud-locate-FN (name)
  "find file by (true) name"
  (find name *file-DB* :key #'plain-name :test #'string=))

(defun cloud-locate-CN (name)
  "find file by (ciper) name"
  (find name *file-DB* :key #'cipher-name :test #'string=))

(defun format-file (DB-rec)
  (format "%S %s %s %s %d %S"
	  (aref DB-rec plain)
	  (aref DB-rec cipher)
	  (aref DB-rec uname)
	  (aref DB-rec gname)
	  (aref DB-rec modes); integer
	  (format-time-string "%04Y-%02m-%02d %H:%M:%S %Z" (aref DB-rec mtime))))

(defun plain-name  (df)(aref df plain))

(defun DBrec-from-file(file-name)
  (when-let ((FA (file-attributes file-name 'string)))
    (let ((DBrec (make-vector (length DB-fields) nil)))
      (destructuring-bind
	  (uid gid acess-time mod-time status-time size ms void inode fsNum)
	  (cddr FA)
	(aset DBrec-from-file uname "unused")
	(aset DBrec-from-file gname gid)
	(aset DBrec-from-file mtime mod-time); list of 4 integers
	(aset DBrec-from-file modes (perms-from-str ms))
	(aset DBrec-from-file plain file-name))
      DBrec)))

;; grab-parameter is no more used as of 2020-09-18
;; (defun grab-parameter (str parname); (grab-parameter "contentsName=z12"  "contentsName") => "z12"
;;   (when (string-match (concat parname "=\\(\\ca+\\)$") str)
;;       (match-string 1 str)))

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
	(aset DB-rec plain FN)
	(aset DB-rec write-me to-cloud); might be later adjusted in read-fileDB
	DB-rec))))))

;; generated from cloud.org
(defvar cloud-delete-contents t "if decrypted contents file must be erased")
(defvar *clouded-hosts* nil "host names participating in file syncronization")
(defvar *pending-actions* nil "actions to be saved in the cloud")
(defvar *removed-files*  nil "files that were just removed (or renamed) on local host before (cloud-sync)")
(defvar *important-msgs* nil "these messages will be typically printed at the end of the process")

(defvar cloud-file-hooks nil "for special files treatment")
(defvar *local-dir* (concat *emacs-d* "cloud/"))

(defvar *local-config* (concat *local-dir* "config"))

(defvar *contents-name* nil)
(defvar *cloud-dir*  "/mnt/cloud/")

(defvar *file-DB* nil "")
(defvar *password* nil); to be read from config or generated
;;(defvar *catalogue* nil); randomly generated name of the catalogue
;;(defvar *encrypted-files* nil "list of the files after they are encrypted")
(defvar DB-fields; indices numerating array fields
(list 'plain; original (local) file name
'cipher; encrypted file name (base name)
'mtime; modification time
'modes; permissions
'uname; user name (obsolete and unused)
'gname; group name
'write-me))
(let ((i 0)) (dolist (field-name DB-fields) (setf i (1+ (set field-name i)))))
(setf to-cloud 1 from-cloud 2)

(defun cloud-context-set-process (context process)
"replaces epg-context-set-process from epg.el"
  (aset context 11 process))
(defun cloud-context-process (context)
"replaces epg-context-process from epg.el"
  (aref context 11))
(defun cloud-wait-for-completion (context)
"replaces epg-wait-for-completion from epg.el"
  (while (eq (process-status (cloud-context-process context)) 'run)
  (sleep-for 0.1)))

(defun launch-encryption (context plain-data cipher-data password)
  (let* ((args (list "--pinentry-mode" "loopback"
                             "--batch" "--yes"
                             "--passphrase" password
                             "-o" (epg-data-file cipher-data)
                             "--symmetric" (epg-data-file plain-data)))
         (buffer (generate-new-buffer " *cloud-crypt*"))
         process)
    (setf process
          (apply #'start-process "cloud" buffer "gpg" args))
    (cloud-context-set-process context process)))
(defun launch-decryption (context plain-data cipher-data password)
  (let* ((args (list "--pinentry-mode" "loopback"
                             "--batch" "--yes"
                             "--passphrase" password
                             "-o" (epg-data-file plain-data)
                             "--decrypt" (epg-data-file cipher-data)))
         (buffer (generate-new-buffer " *cloud-crypt*"))
         process)
    (setf process
          (apply #'start-process "cloud" buffer "gpg" args))
    (cloud-context-set-process context process)))

(defun end-log (fstr &rest args)
  "message + time"
  (push
   (apply #'format
          (cons (concat
                 (format-time-string "%H:%M:%S " (apply 'encode-time (butlast (decode-time (current-time)) 3)))
                 fstr)
                args))
   *important-msgs*))

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

(defvar do-not-encrypt '("gpg"))

(defun cloud-encrypt (plain-file cipher-file password)
(let ((cloud-name (concat *cloud-dir* cipher-file ".gpg")))
(if (member (file-name-extension plain-file) do-not-encrypt)
    (progn (copy-file plain-file cloud-name t) t)
  (let (sucess (context (epg-make-context 'OpenPGP)))
    (launch-encryption context 
                       (epg-make-data-from-file plain-file)
                       (epg-make-data-from-file cloud-name)
                       password)
    (cloud-wait-for-completion context)
    (setf sucess (= 0 (process-exit-status (cloud-context-process context))))
    (epg-reset context); closes the buffer (among other things)
    sucess))))
(defun cloud-decrypt (cipher-file plain-file password)
  (let ((cloud-name (clouded cipher-file))
        (dir (file-name-directory plain-file)))
    (unless (file-directory-p dir) (make-directory dir t))
  (if (member (file-name-extension plain-file) do-not-encrypt)
      (progn (copy-file cloud-name plain-file t) t)
    (let (sucess (context (epg-make-context 'OpenPGP)))
      (launch-decryption context
                         (epg-make-data-from-file plain-file)
                         (epg-make-data-from-file cloud-name)
                         password)
      (cloud-wait-for-completion context)
      (when (setf sucess (= 0 (process-exit-status (cloud-context-process context))))
        (post-decrypt plain-file))
      (epg-reset context); closes the buffer (among other things)
      sucess))))

(defun cloud-connected-p()
  (and
   *cloud-dir* *contents-name*
   (file-readable-p *cloud-dir*)))
;;(file-readable-p (concat *cloud-dir* *contents-name* ".gpg")

(defun write-conf()
(with-temp-file *local-config*
  (insert (format "delete-contents=%s" (if cloud-delete-contents "yes" "no"))) (newline)
  (insert (format "contents-name=%s" *contents-name*)) (newline)
  (insert (format "password=%s" *password*)) (newline)
  (insert (format "cloud-directory=%s" *cloud-dir*)) (newline)))

(defun cloud-init() "initializes cloud directory and generates password -- runs only once"
(interactive)
(when (yes-or-no-p "Is cloud mounted?")
(setf *cloud-dir* (read-string "cloud directory=" *cloud-dir*))
(ifn (member (safe-mkdir *cloud-dir*) '(:exists t))
(clog :error "could not create/acess directory %s" *cloud-dir*)

(if (directory-files *cloud-dir* nil "^.\+.gpg$" t)
    (clog :error "please clean the directory %s before asking me to initialize it" *cloud-dir*)
(clog :info "creating (main) contents file in unused directory %s" *cloud-dir*)
(ifn-set ((*contents-name* (new-file-name *cloud-dir*)))
  (clog :error "could not create DB file in the directory %s" *cloud-dir*)

(setf *password* (rand-str 9))

(ifn (member (safe-mkdir *local-dir*) '(:exists t))
(clog :error "could not create/acess directory %s" *local-dir*)
(write-conf)
(clog :info "use M-x cloud-add in the dired to cloud important files and directories" )))))))

(defun write-fileDB (DBname)
  (with-temp-file DBname
(dolist (hostname *clouded-hosts*) (insert (format "%s " hostname)))
(delete-char -1) (newline)

(dolist (action *pending-actions*)
  (insert (format-action action)) (drop *pending-actions* action) (delete-char -1) (newline))

(newline)

(dolist (file-record *file-DB*)
  (insert (format-file file-record)) (newline))
(setf *removed-files* nil)))

(defun clouded(CN) (concat *cloud-dir* CN ".gpg"))

(defun read-fileDB* (DBname)
  "reads content (text) file into the database *file-DB*"
  (find-file DBname) (goto-char (point-min))
(macrolet ((read-line() '(setf str (buffer-substring-no-properties (point) (line-end-position)))))
  (let ((BN (buffer-name)) str)
(needs-set
 ((*clouded-hosts* 
  (split-string (read-line))
  (clog :error "invalid first line in the contents file %s" DBname)))

(unless (member (system-name) *clouded-hosts*) (cloud-host-add))
(forward-line)

(while (< 0 (length (read-line)))
(let ((action (make-vector (length action-fields) nil)))

(dolist (column (list
                 `(:time-stamp . ,i-time)
                 `(:int . ,i-ID)
                 `(:int . ,i-Nargs)))
  (needs ((col-value (begins-with str (car column)) (bad-column "action" (cdr column))))
     (aset action (cdr column) (car col-value))
     (setf str (cdr col-value))))

(dolist (column (list
                 (cons (cons  :string  (aref action i-Nargs)) i-args)
                 `(:strings . ,i-hostnames)))
  (needs ((col-value (begins-with str (car column)) (bad-column "action" (cdr column))))
     (aset action (cdr column) (car col-value))
     (setf str (cdr col-value))))

(let ((AID (format-time-string "%02m/%02d %H:%M:%S" (aref action i-time))))
  (ifn (member (system-name) (aref action i-hostnames))
      (clog :info "this host is unaffected by action %s" AID)
    (when (perform action)
        (clog :debug "sucessfully performed action %s" AID)
      (clog :error " action %s failed, will NOT retry it" AID))

(when (drop (aref action i-hostnames) (system-name))
  (push action *pending-actions*)))))
  (forward-line))

(forward-line)
(while (< 10 (length (read-line)))
(let ((CF (make-vector (length DB-fields) nil)))
  (ifn (string-match "\"\\(.+\\)\"\s+\\([^\s]+\\)\s+\\([^\s]+\\)\s+\\([^\s]+\\)\s+\\([[:digit:]]+\\)\s+\"\\(.+\\)\"" str)
  (clog :error "ignoring invalid file-line %s in the contents file %s" str DBname)

(let* ((FN (match-string 1 str)))
  (aset CF plain FN)
  (aset CF cipher (match-string 2 str))
  (aset CF uname (match-string 3 str))

(aset CF gname (match-string 4 str))
  (aset CF modes (string-to-int (match-string 5 str)))
  (let ((mtime-str (match-string 6 str)))
(ifn (string-match "[0-9]\\{4\\}-[0-9][0-9]-[0-9][0-9] [0-9][0-9]:[0-9][0-9]:[0-9][0-9] [[:upper:]]\\{3\\}" mtime-str)
(bad-column "file" 6 mtime-str)
(aset CF mtime (parse-time mtime-str))))
(ifn-let ((LF (cloud-locate-FN FN)))
(push (setf LF CF) *file-DB*)

(let ((local-exists (file-exists-p FN)) (remote-exists (file-exists-p (clouded (cipher-name CF)))))
(cond
((not (or local-exists remote-exists))
 (clog :error "forgetting file %s which is marked as clouded but is neither on local disk nor in the cloud" FN)
 (drop *file-DB* LF CF))
((and local-exists remote-exists)
(aset LF write-me (cond
 ((time< (aref LF mtime) (aref CF mtime)) from-cloud)
 ((time< (aref CF mtime) (aref LF mtime)) to-cloud)
 (t 0))))
(local-exists  (aset LF write-me to-cloud))
(remote-exists (unless (member LF *removed-files*)
(aset LF write-me from-cloud)))))))))

(forward-line)))
(kill-buffer BN))))

(defmacro bad-column (cType N &optional str)
(if str
`(clog :error "invalid %dth column in %s line = %s" ,N ,cType ,str)
`(clog :error "invalid %dth column in %s line" ,N ,cType)))

(defun on-current-buffer-save ()
  "attention: this function might be called many times within a couple of seconds!"
  (let ((plain-file (file-chase-links (buffer-file-name))))
(when (and plain-file (stringp plain-file))
  (let ((file-data (cloud-locate-FN plain-file)))
    (when file-data
      (aset file-data mtime (current-time))
      (aset file-data write-me to-cloud))))))
(add-hook 'after-save-hook 'on-current-buffer-save)

(defun cloud-sync()
(interactive)
(let* ((lockdir (concat *cloud-dir* "now-syncing/"))
(lockfile (concat lockdir (system-name)))
(time-stamp (TS (current-time))))
(ifn (safe-mkdir lockdir)
(clog :error "lock directory %s exists; someone else might be syncing right now. If this is not the case, remove %s manually" lockdir lockdir)
(write-region time-stamp nil lockfile)
(let ((ok t))
  (ifn (cloud-connected-p)
      (clog :error "cloud-sync header failed")
    (clog :info "started syncing")

(read-fileDB)

(dolist (FD *file-DB*)
(when ok
(unless (aref FD write-me) (aset FD write-me 0))
(case= (aref FD write-me)
(from-cloud
(when (and
(if (= 0 *log-level*) (yes-or-no-p (format "replace the file %s from the cloud?" (aref FD plain))) t)
(progn (clog :debug "Next call = cloud-decrypt(%s,%s)" (cipher-name FD) (plain-name FD)) t)
(setf ok (cloud-decrypt (cipher-name FD) (plain-name FD) *password*)))
   (clog :info "cloud/%s.gpg --> %s" (cipher-name FD) (plain-name FD))
   (set-file-modes (plain-name FD) (aref FD modes))
   (set-file-times (plain-name FD) (aref FD mtime))
   (chgrp (aref FD gname) (plain-name FD)); I have to call external program in order to change the group
   (aset FD write-me 0)
   (needs ((hooks (assoc (plain-name FD) cloud-file-hooks)))
(dolist (hook hooks) 
              (funcall (cdr hook) (car hook))))))

(to-cloud
   (when (cloud-encrypt (plain-name FD) (cipher-name FD) *password*)
     (clog :info "%s (%s) --> cloud:%s.gpg"
       (plain-name FD)
       (TS (aref FD mtime))
       (cipher-name FD))
     (aset FD write-me 0))))))
(ifn ok (progn
(end-log "error (en/de)crypting files, cloud-sync aborted")
(clog :error "error (en/de)crypting files, cloud-sync aborted"))
(let ((tmp-CCN (concat *local-dir* "CCN")))
   (write-fileDB tmp-CCN)
   (if (setf ok (cloud-encrypt tmp-CCN *contents-name* *password*))
       (when cloud-delete-contents (safe-dired-delete tmp-CCN))
     (clog :error "failed to encrypt content file %s to %s!" tmp-CCN *contents-name*))))

(dolist (msg (reverse *important-msgs*)) (message msg))
(clog :info "done syncing")
ok))
(ifn (and (safe-delete-file lockfile) (safe-delete-dir lockdir))
(clog :error "could not delete lock file %s and directory %s" lockfile lockdir)
(write-region (format "%s: %s
" (system-name) time-stamp) nil (concat *cloud-dir* "history") t)))))

(defvar action-fields '(i-time i-ID i-args i-hostnames i-Nargs))
(let ((i 0)) (dolist (AF action-fields) (setf i (1+ (set AF i)))))

(defvar action-IDs '(i-forget i-delete i-rename i-host-add i-host-forget))
(let ((i 0)) (dolist (AI action-IDs) (setf i (1+ (set AI i)))))

(defun new-action (a-ID &rest args)
  (let ((action (make-vector (length action-fields) nil)))
    (aset action i-ID a-ID)
    (aset action i-time (current-time))
    (aset action i-args args)
    (aset action i-hostnames (remove (system-name) *clouded-hosts*))
    (push action *pending-actions*)))

(defun perform(action)
  (let ((arguments (aref action i-args)))
    (case= (aref action i-ID)
      (i-host-forget (dolist (arg arguments) (drop *clouded-hosts* arg)) t)
      (i-host-add (dolist (arg arguments) (push arg *clouded-hosts*)) t)
      (i-forget (cloud-forget-many arguments) t)
      (i-delete (cloud-rm arguments) t)
      (i-rename (cloud-rename-file (first arguments) (second arguments)) t)
      (otherwise (clog :error "unknown action %d" (aref action i-ID)))))
   (drop *pending-actions* action) t)

(defun format-action (action)
  (format "%S %d %d %s %s"
(TS (aref action i-time)); 1. Time stamp,
(aref action i-ID); 2. (integer) action ID,
(length (aref action i-args)); 3. (integer) number of arguments for this action (one column),
(apply #'concat (mapcar #'(lambda(arg) (format "%S " arg)) (aref action i-args))); 4. [arguments+] (several columns),
(apply #'concat (mapcar #'(lambda(HN) (format "%S " HN)) (aref action i-hostnames))))); 5. hostnames, where the action has to be performed (several columns).

(unless (boundp 'DRF) (defvar DRF (indirect-function (symbol-function 'dired-rename-file)) "original dired-rename-file function"))
(unless (boundp 'DDF) (defvar DDF (indirect-function (symbol-function 'dired-delete-file)) "original dired-delete-file function"))

(defun dired-delete-file (FN &optional dirP TRASH)
  (let (failure)

(condition-case err (funcall DDF FN dirP TRASH)
  (file-error
    (clog :error "in DDF: %s" (error-message-string err))
    (setf failure t)))
(unless failure

(cloud-forget-recursive FN) (new-action i-delete FN)
(when dirP
  (dolist (sub-FN (mapcar #'plain-name (contained-in FN)))
    (when (cloud-forget-file sub-FN) (new-action i-delete sub-FN)))))))

(defun cloud-rm (args)
  (interactive) 
(let ((ok (cloud-forget-many args)))
  (dolist (arg args)
    (setf ok (and (safe-dired-delete arg) (cloud-forget-recursive arg) ok)))
ok))

(defun cloud-forget-many (args)
  (interactive) 
(let ((ok t))
  (dolist (arg args)
    (setf ok (and (cloud-forget-recursive arg) ok)))
ok))

(defun cloud-delete-file (local-FN)
  (needs ((DB-rec (cloud-locate-FN local-FN) (clog :info "delete: doing nothing since %s is not clouded")))
    (new-action i-delete local-FN)
    (drop *file-DB* DB-rec)
    (safe-dired-delete (concat *cloud-dir* (aref DB-rec cipher) ".gpg"))))

(defun contained-in(dir-name); dir-name must end with a slash /
    (let (res)
      (dolist (DB-rec *file-DB*)
        (when(and
(< (length dir-name) (length (aref DB-rec plain)))
(string=(substring-no-properties (aref DB-rec plain) 0 (length dir-name)) dir-name))
          (push DB-rec res)))
      res))

(defun add-to-actions(hostname)
  (dolist (action *pending-actions*)
    (unless (member hostname (aref action i-hostnames))
      (aset action i-hostnames (cons hostname (aref action i-hostnames))))))
(defun erase-from-actions(hostname)
  (dolist (action *pending-actions*)
    (when (member hostname (aref action i-hostnames))
      (aset action i-hostnames (remove hostname (aref action i-hostnames))))))

(defun cloud-host-add ()
  "adding THIS host to the cloud sync-system"
(let ((hostname (system-name)))
  (unless (member hostname *clouded-hosts*)
    (push hostname *clouded-hosts*))
  (new-action i-host-add hostname)
  (add-to-actions hostname)))

(defun cloud-host-forget ()
  "remove host from the cloud sync-system"
  (let ((hostname (system-name)))
    (when (yes-or-no-p (format "Forget the host %s?" hostname))
      (new-action i-host-forget hostname)
      (if (cloud-sync)
          (safe-dired-delete *local-config*)
        (clog :error "sync failed, so I will not erase local configuration")))))

(defun cloud-add (&optional FN)
  (interactive)
  (if (string= major-mode "dired-mode")
      (dired-map-over-marks (add-files (dired-get-filename)) nil)
    (unless
        (add-files (read-string "file to be clouded=" (if FN FN "")))
      (clog :error "could not cloud this file"))))

(defun cloud-forget-file (local-FN); called *after* the file has already been sucessfully deleted
   (push local-FN *removed-files*)
  (needs ((DB-rec (cloud-locate-FN local-FN) (clog :info "forget: doing nothing since %s is not clouded" local-FN))
          (cloud-FN (concat  *cloud-dir* (aref DB-rec cipher) ".gpg") (clog :error "in DB entry for %s" local-FN)))
   (drop *file-DB* DB-rec)
   (push local-FN *removed-files*)
   (safe-dired-delete cloud-FN) t))

(defun cloud-forget-recursive(FN); called *after* the file has already been sucessfully deleted
(dolist (sub-FN (mapcar #'plain-name (contained-in FN)))
(cloud-forget-file sub-FN)))

(defun cloud-forget (&optional FN)
  (interactive)
  (if (string= major-mode "dired-mode")
      (dired-map-over-marks (cloud-forget-recursive (dired-get-filename)) nil)
    (unless
        (cloud-forget-recursive (read-string "file to be forgotten=" (if FN FN "")))
      (clog :error "could not forget this file"))))

(defun cloud-rename-file (old new)
  (let ((source (cloud-locate-FN old))
        (target (cloud-locate-FN new)))
(cloud-forget-recursive old)
    (cond
     ((and source target); overwriting one cloud file with another one
      (loop for property in (list mtime modes uname gname write-me) do
            (aset target property (aref source property)))
      (drop *file-DB* source))
     (source (aset source plain new))
     (target (setf target (get-file-properties new))))))

(defun dired-rename-file (old-FN new-FN ok-if-already-exists)
  (let (failure)
    (clog :debug "DRF")
    (condition-case err
        (funcall DRF old-FN new-FN ok-if-already-exists)
      (file-error
       (clog :debug "DRF error!")
       (message "%s" (error-message-string err))
       (setf failure t)))
    (unless failure
      (clog :debug "launching my cloud rename %s --> %s" old-FN new-FN)
      (cloud-rename-file old-FN new-FN)
      (new-action i-rename old-FN new-FN))))

(defun cloud-start()
  (interactive) (save-some-buffers)
(clog :debug "cloud-start: *local-config* = %s" *local-config*)
(if-let ((conf (read-conf *local-config*)))
    (ifn (and
          (if-let ((CD (cdr (assoc "cloud-directory" conf))))
                  (setf *cloud-dir* CD); "/mnt/lws/cloud/"
                  (setf *cloud-dir* (read-string "cloud directory=" *cloud-dir*))
                  (write-conf) t)
(progn (when-let ((delete-contents (cdr (assoc "delete-contents" conf))))
          (setf cloud-delete-contents (if (string= "no" delete-contents) nil t)))t)
          (setf *contents-name* (cdr (assoc "contents-name" conf)))
          (setf *password*  (cdr (assoc "password" conf))))
         (clog :error "cloud-start header failed, consider (re)mounting %s or running (cloud-init)" *cloud-dir*)
         (cloud-sync))
    (clog :warning "could not read local configuration file")
    (when (yes-or-no-p "(Re)create configuration?")
      (cloud-init))))

(defun read-fileDB()
  (let ((tmp-CCN (concat *local-dir* "CCN")))
(or
(and
 (cloud-connected-p)
 (cloud-decrypt *contents-name* tmp-CCN *password*)
 (progn (read-fileDB* tmp-CCN)
        (if cloud-delete-contents
            (safe-dired-delete tmp-CCN) t)))
(progn (clog :error "cloud-start header failed") nil))))

(defun read-conf (file-name)
  "reads configuration file"
(clog :debug "read-conf")
  (find-file *local-config*) (goto-char (point-min)); opening config file
  (let (res str (BN (buffer-name)))
    (while (and
            (setf str (buffer-substring-no-properties (point) (line-end-position)))
            (< 0 (length str)))
     (if (string-match "^\\(\\ca+\\)=\\(\\ca+\\)$" str)
         (push (cons (match-string 1 str) (match-string 2 str)) res)
       (clog :error "garbage string in configuration file: %s" str))
(forward-line))
(kill-buffer BN)
    res))

(unless (boundp '*loaded*)
  (defvar *loaded* nil)); actually supposed to be diefined in ~/.emacs
(provide 'cloud)
;;; cloud.el ends here
