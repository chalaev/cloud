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

(defun parse-time(str)
  (let* ((TPD (timezone-parse-date str)) (TZ (aref TPD 4))) ;["2020" "10" "10" "14:54:40" "MSK"]
    (unless (assoc TZ timezone-world-timezones)
      (clog :error "Unknown time zone abbreviation %s, update 'timezone-world-timezones' variable" TZ))
    (apply #'encode-time (append
			  (firstN
			   (parse-time-string
			    (format "%s-%s-%s %s" (aref TPD 0) (aref TPD 1) (aref TPD 2) (aref TPD 3)))
			   8)
			  (list (* 60 (timezone-zone-to-minute TZ)))))))

;; test: (format-time-string "%04Y-%02m-%02d %H:%M:%S %Z" (parse-time "2020-10-10 14:54:40 MSK"))

;;(parse-time "2019-09-05 16:09:37 EDT")
