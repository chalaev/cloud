;;; -*- mode: Emacs-Lisp;  lexical-binding: t; -*-

;; Instead of using this file, one could use functions.el
;; generated from org-file in elisp-goodies project, see
;; https://github.com/chalaev/elisp-goodies

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
