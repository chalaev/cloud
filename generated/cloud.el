;;; -*- mode: Emacs-Lisp;  lexical-binding: t; -*-
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

(defun full-TS (time)
  (format-time-string "%F %H:%M:%S %Z" time))

(defun TS (time)
  (format-time-string "%02m/%02d %H:%M:%S" time))

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

(defun parse-date (str)
  (mapcar 'string-to-int 
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

(defun chgrp(group file-name)
  (= 0 (call-process "chgrp" nil nil nil group file-name)))

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

(defun firstN(lista N)
  "returning first N elments of the list"
  (when (and (< 0 N) (car lista))
    (cons (car lista) (firstN (cdr lista) (1- N)))))
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
  (push msg *file-acc-buffer*)
  (when (< 30 (length *file-acc-buffer*)) (clog-flush)))

(defun clog (level fstr &rest args)
  "simple logging function" ; level is one of → :debug :info :warning :error
  (when (<= *log-level* (or (pos level '(:debug :info :warning :error)) 0))
    (let ((log-msg
           (cons
            (concat "%s " (format-time-string "%H:%M:%S "
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
  (let (failed)
    (condition-case err (funcall DDF FN "always")
      (file-error
       (clog :error "in DDF: %s" (error-message-string err))
       (setf failed t)))
    (not failed)))

(defun safe-delete-file (FN)
  (let (failed)
  (condition-case err (delete-file FN)
    (file-error
     (clog :error "cannot delete file %s; %s" FN (error-message-string err))
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
;;    (cons (apply #'encode-time (parse-time-string (car res))) (cdr res))))
    (cons (apply #'encode-time (parse-date-time (car res))) (cdr res))))
   ((eql :strings what)
    (let (BW result)
      (while (setf BW (begins-with* str :string))
	(push (car BW) result)
	(setf str (cdr BW)))
      (cons (reverse result) str)))
   (t (begins-with* str what))))

(defun tilda(x) (replace-regexp-in-string (concat "^" ~) "~" x))
(defun untilda(x) (replace-regexp-in-string "^~" ~ x))

(defun cloud-locate-FN (name)
  "find file by (true) name"
  (find name *file-DB* :key #'plain-name
	:test #'(lambda(x y)(string= (tilda x) (tilda y)))))

(defun cloud-locate-CN (name)
  "find file by (ciper) name"
  (find name *file-DB* :key #'cipher-name :test #'string=))

(defun format-file (DB-rec)
  (format "%S %s %s %s %d %S"
	  (tilda (aref DB-rec plain))
	  (aref DB-rec cipher)
	  (aref DB-rec uname)
	  (aref DB-rec gname)
	  (aref DB-rec modes); integer
	  (format-time-string "%F %H:%M:%S %Z" (aref DB-rec mtime))))

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
	(aset DB-rec plain FN); (aset DB-rec write-me to-cloud); might be later adjusted in read-fileDB
	DB-rec))))))
;;; -*- mode: Emacs-Lisp;  lexical-binding: t; -*-

;; generated from cloud.org
(defvar cloud-delete-contents t "if decrypted contents file must be erased")
(defvar *clouded-hosts* nil "host names participating in file syncronization")
(defvar *pending-actions* nil "actions to be saved in the cloud")
(defvar *removed-files*  nil "files that were just removed (or renamed) on local host before (cloud-sync)")
(defvar *important-msgs* nil "these messages will be typically printed at the end of the process")
(defvar ~ (getenv "HOME"))
(defvar *gpg-process* nil "assyncronous make-process for (en/de)cryption")

(defvar cloud-file-hooks nil "for special files treatment")
(defvar *local-dir* (concat *emacs-d* "cloud/"))
(defvar *local-log* (concat *local-dir* (system-name) ".log"))
(defvar *cloud-mk* (concat *local-dir* "cloud.mk"))
(defvar *Ncores* 1)
(defvar *cloud-dir*  "/mnt/cloud/")
(defvar cloud-lockdir (concat *cloud-dir* "now-syncing/"))
(defvar cloud-lockfile (concat cloud-lockdir (system-name)))

(defvar *local-config* (concat *local-dir* "config"))

(defvar *contents-name* nil)

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
))
(let ((i 0)) (dolist (field-name DB-fields) (setf i (1+ (set field-name i)))))

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
  (insert (format "number-of-CPU-cores=%s" *Ncores**password)) (newline)
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

(dolist (action (reverse *pending-actions*))
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
(clog :info "action %s ..." str)
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
     (aset action (cdr column) (car col-value)); was (mapcar #'untilda (car col-value))
     (setf str (cdr col-value))))

(let ((AID (format-time-string "%02m/%02d %H:%M:%S" (aref action i-time))))
(clog :info "... will later be referenced as %s" AID)
  (ifn (member (system-name) (aref action i-hostnames))
      (clog :info "this host is unaffected by action %s" AID)
    (when (perform action)
        (clog :debug "sucessfully performed action %s" AID)
      (clog :error " action %s failed, will NOT retry it" AID))

(when (drop (aref action i-hostnames) (system-name))
  (push action *pending-actions*)))))
  (forward-line))

(reset-Makefile)
(forward-line)
(while (< 10 (length (read-line)))
(when-let ((CF (str-to-DBrec str)))

(let* ((FN (plain-name CF))
      (remote-exists (file-exists-p (clouded (cipher-name CF))))
      (local-exists (or (cloud-locate-FN FN)
(when-let ((LF (get-file-properties FN)))
        (aset LF cipher (aref CF cipher))
        (push LF *file-DB*)
        LF))))
(cond
((not (or local-exists remote-exists))
 (clog :error "forgetting file %s which is marked as clouded but is neither on local disk nor in the cloud" FN)
 (drop *file-DB* CF))
((or
 (and (not local-exists) remote-exists)
 (and local-exists remote-exists (time< (aref local-exists mtime) (aref CF mtime))))
(download CF))
((or
 (and local-exists remote-exists (time< (aref CF mtime) (aref local-exists mtime)))
 (and local-exists (not remote-exists)))
(upload CF)))))

(forward-line))

(save-Makefile) (kill-buffer BN)))))

(defmacro bad-column (cType N &optional str)
(if str
`(clog :error "invalid %dth column in %s line = %s" ,N ,cType ,str)
`(clog :error "invalid %dth column in %s line" ,N ,cType)))

(defun on-current-buffer-save ()
  "attention: this function might be called many times within a couple of seconds!"
  (let ((plain-file (file-chase-links (buffer-file-name))))
(when (and plain-file (stringp plain-file))
  (when-let ((file-data (cloud-locate-FN plain-file)))
  (aset file-data mtime (current-time))))))
(add-hook 'after-save-hook 'on-current-buffer-save)

(defun str-to-DBrec(str)
"parses one file line from the contents file"
(ifn (string-match "\"\\(.+\\)\"\s+\\([^\s]+\\)\s+\\([^\s]+\\)\s+\\([^\s]+\\)\s+\\([[:digit:]]+\\)\s+\"\\(.+\\)\"" str)
(clog :error "Ignoring invalid file line %s" str)

(let ((CF (make-vector (length DB-fields) nil))
      (FN (match-string 1 str)))
  (aset CF plain FN)
  (aset CF cipher (match-string 2 str))
  (aset CF uname (match-string 3 str))

(aset CF gname (match-string 4 str))
  (aset CF modes (string-to-int (match-string 5 str)))
  (let ((mtime-str (match-string 6 str)))
(ifn (string-match "[0-9]\\{4\\}-[0-9][0-9]-[0-9][0-9] [0-9][0-9]:[0-9][0-9]:[0-9][0-9] [[:upper:]]\\{3\\}" mtime-str)
(bad-column "file" 6 mtime-str)
(aset CF mtime (parse-time mtime-str))
CF)))))

(let (all Makefile)
(macrolet ((NL () '(push "
" Makefile))
(inl (&rest format-pars) `(progn (push ,(cons 'format format-pars) Makefile) (NL))))

(defun download (file-record)
(ifn-let ((FN (plain-name file-record)))
(clog :error "upload: file has no plain name!")
(push (format " %s" FN) all)
(inl (concat "%s: $(cloud)%s.gpg
\t" (if (member (file-name-extension FN) do-not-encrypt)
"cp $< $@" "$(dec) $@ $<")
"
\t-chgrp %s $@
\t-chmod %o $@
\t-touch --date=%S $@
\t-echo \"`date '+%%m/%%d %%T'`: downloaded %s\" >> %s
") FN (cipher-name file-record) 
(aref file-record gname) (aref file-record modes) (full-TS (aref file-record mtime))
FN *local-log*)))

(defun upload (file-record)
(ifn-let ((FN (plain-name file-record)))
(clog :error "upload: file has no plain name!")
(push (format " $(cloud)%s.gpg" (cipher-name file-record)) all) (inl
(concat "$(cloud)%s.gpg: %s
\t"
(if (member (file-name-extension FN) do-not-encrypt)
"cp $< $@" "$(enc) $@ $<")
"
\t-echo \"`date '+%%m/%%d %%T'`: uploaded %s\" >> %s
") (cipher-name file-record) FN 
FN *local-log*)))

(defun reset-Makefile()
"reseting make file"
(setf all nil Makefile nil)
(inl "cloud=%s" *cloud-dir*)
(inl "password=%s" *password*)
(inl "gpg=gpg --pinentry-mode loopback --batch --yes")
(inl "enc=$(gpg) --symmetric --passphrase $(password) -o")
(inl "dec=$(gpg) --decrypt   --passphrase $(password) -o
"))

(defun save-Makefile()
"flushing make file"
(inl "all:%s
\techo \"background (en/de)cryption on %s finished `date '+%%m/%%d %%T'`\" >> %s
\t-rm %s
\t-rmdir %s
"
(apply #'concat all)
(system-name)
(concat *cloud-dir* "history")
cloud-lockfile cloud-lockdir)
(write-region (apply #'concat (reverse Makefile)) nil *cloud-mk*)
(chgrp "tmp" *cloud-mk*))))

(defun cloud-sync()
(interactive)
(let ((time-stamp (TS (current-time)))
      (mkdir (safe-mkdir cloud-lockdir)) (ok t))
(cond
((not mkdir) (clog :error "can not create lock directory %s. Is the remote directory monted?" cloud-lockdir))
((member mkdir '(:exists))
       (clog :error "lock directory %s exists; someone else might be syncing right now. If this is not the case, remove %s manually" cloud-lockdir cloud-lockdir))
((and *gpg-process* (process-live-p *gpg-process*))
(clog :error "I will not start new (en/de) coding process because the previous one is still funning"))
((not (cloud-connected-p)) (clog :error "remote directory is not mounted"))
((progn (write-region time-stamp nil cloud-lockfile) (read-fileDB))
   (clog :info "started syncing")
(if (and *gpg-process* (process-live-p *gpg-process*))
(clog :error "I will not start new (en/de) coding process because the previous one is still funning")
(setf *gpg-process* (apply #'start-process (append (list
"cloud-batch" 
(generate-new-buffer "*cloud-batch*")
"make")
(split-string (format "-j%d -f %s all" *Ncores* *cloud-mk*))))))

(let ((tmp-CCN (concat *local-dir* "CCN")))
   (write-fileDB tmp-CCN)
   (if (setf ok 
(= 0 (apply #'call-process
(append (list "gpg" nil nil nil)
(split-string (format
"--batch --yes --pinentry-mode loopback --passphrase %s  -o %s --symmetric %s"
*password* (clouded *contents-name*)  tmp-CCN))))))
       (when cloud-delete-contents (safe-dired-delete tmp-CCN))
     (clog :error "failed to encrypt content file %s to %s!" tmp-CCN *contents-name*))))
(t (clog :error "unknown error in cloud-sync")))

(dolist (msg (reverse *important-msgs*)) (message msg))
(setf *important-msgs* nil)
(clog :info "done syncing")
(clog :debug "will now erase %s and %s" cloud-lockfile cloud-lockdir)
(ifn (and (safe-delete-file cloud-lockfile) (safe-delete-dir cloud-lockdir))
     (clog :error "could not delete lock file %s and directory %s" cloud-lockfile cloud-lockdir)
     (write-region (format "%s: %s -- %s
" (system-name) time-stamp (format-time-string "%H:%M:%S" (current-time))) nil (concat *cloud-dir* "history") t))
ok))

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
(write-region
(format "%s: %s
" (TS (current-time)) (format-action action))
nil local-log t)
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
(full-TS (aref action i-time)); 1. Time stamp,
(aref action i-ID); 2. (integer) action ID,
(length (aref action i-args)); 3. (integer) number of arguments for this action (one column),
(apply #'concat (mapcar #'(lambda(arg) (format "%S " (tilda arg))) (aref action i-args))); 4. [arguments+] (several columns),
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
      (loop for property in (list mtime modes uname gname) do
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
(progn
(setf cloud-lockdir (concat *cloud-dir* "now-syncing/"))
(setf cloud-lockfile (concat cloud-lockdir (system-name)))
 (when-let ((delete-contents (cdr (assoc "delete-contents" conf))))
          (setf cloud-delete-contents (if (string= "no" delete-contents) nil t)))t)
          (setf *contents-name* (cdr (assoc "contents-name" conf)))
(setf *Ncores* (string-to-number (or (cdr (assoc "number-of-CPU-cores" conf)) "1")))
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
(= 0 (apply #'call-process
(append (list "gpg" nil nil nil)
(split-string (format
"--batch --yes --pinentry-mode loopback --passphrase %s -o %s --decrypt %s"
*password* tmp-CCN  (clouded *contents-name*))))))
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
  (defvar *loaded* nil)); actually supposed to be defined in ~/.emacs
(provide 'cloud)
;;; cloud.el ends here
