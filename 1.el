;; 1.el

(defun backspace()
  (if (< (point-min) (point))
      (delete-char -1)
    (clog :error "can not backspace in buffer(%s), file(%s)"
	  (buffer-name)
	  (if-let ((FN (buffer-file-name))) FN "N/A"))))

(defun new-file-in(cloud-dir)
  (let(new-fname error-exists)
    (cl-loop repeat 10 do (setf new-fname (rand-str 3))
          while (setf error-exists (file-exists-p (concat cloud-dir new-fname))))
    (if error-exists nil new-fname)))

(defun begins-with*(str what)
  (let ((ok t))
  (needs ((pattern
	   (cl-case what; [\s-\|$] matches space or EOL
	     (:time "\s*\"\\([^\"]+\\)\"[\s-\|$]")
	     (:int "\s+\\([[:digit:]]+\\)[\s-\|$]")
	     (:string "[\s-]*\"\\(.+?\\)\"")
	     (:other "\\([^\s-]+\\)"))
	   (clog :error "invalid type %s in begins-with" what))
	  (MB (when (string-match pattern str) (match-beginning 1)))
	  (ME (match-end 1))
	  (matched (match-string 1 str)))
	 (cl-case what
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

(defun begins-with(str what)
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
    (cons (apply #'encode-time (parse-date-time (car res))) (cdr res))))
   ((eql :strings what)
    (let (BW result)
      (while (setf BW (begins-with* str :string))
	(push (car BW) result)
	(setf str (cdr BW)))
      (cons (reverse result) str)))
   ((eql :others what)
    (let (BW result)
      (while (setf BW (begins-with* str :other))
	(push (car BW) result)
	(setf str (cdr BW)))
      (cons (reverse result) str)))
   (t (begins-with* str what))))

(defun cloud-locate-FN(FN)
  "find file by (true) name"
(when FN  
  (cl-find (file-chase-links FN) file-DB :key #'plain-name
	:test #'(lambda(x y)(string= (tilde x) (tilde y))))))

(defun cloud-locate-CN(name)
  "find file by (ciper) name"
  (cl-find name file-DB :key #'(lambda(x)(aref x cipher)) :test #'string=))

(defun cloud-get-file-properties(FN)
  (when-let((FP(get-file-properties FN)))
    (vconcat FP [nil])))

 ;; Note sure if the following function is necessary, or, may be, it should be declared as macro or "inline":
(defun plain-name (df)(aref df plain))
