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
