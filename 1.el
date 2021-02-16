;; -*- mode: Emacs-Lisp;  lexical-binding: t; -*-

;; I try to get rid of loop and other common-lisp stuff here

;;(setf coding-system-for-read 'utf-8)
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
;; 	:test #'(lambda(x y)(string= (tilde x) (tilde y)))))

(defun cloud-locate-FN (FN)
  "find file by (true) name"
(when FN  
  (find (file-chase-links FN) file-DB :key #'plain-name
	:test #'(lambda(x y)(string= (tilde x) (tilde y))))))

(defun cloud-locate-CN (name)
  "find file by (ciper) name"
  (find name file-DB :key #'cipher-name :test #'string=))

(defun cloud-get-file-properties(FN)
  (when-let((FP(get-file-properties FN)))
    (vconcat FP [nil])))

 ;; Note sure if the following 2 functions are necessary, or, may be, they should be declared as macro or "inline":
(defun plain-name  (df)(aref df plain))
(defun cipher-name (df)(aref df cipher))

;; grab-parameter is no more used as of 2020-09-18
;; (defun grab-parameter (str parname); (grab-parameter "contentsName=z12"  "contentsName") => "z12"
;;   (when (string-match (concat parname "=\\(\\ca+\\)$") str)
;;       (match-string 1 str)))

(defun cat-file(FN)
"converts file to string"
(with-temp-buffer
    (insert-file-contents FN)
    (buffer-string)))

(defun together(strings)
(if strings
  (mapconcat 'identity strings " ")
  ""))

(defmacro report-TF(var-name)
  "useful for debugging"
  `(clog :debug (concat ,(symbol-name var-name) "= %s") (if ,var-name "t" "nil")))
;; example: (report-TF file-DB)

;; (defmacro report-TF(var-name)
;;   "useful for debugging"
;;   (let ((v (s-gensym)))
;;   `(let ((,v ,var-name))
;;      (clog :debug (concat ,(symbol-name v) "= %s") (if ,v "t" "nil")))))
