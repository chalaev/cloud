;; -*- mode: Emacs-Lisp;  lexical-binding: t; -*-

(load "~/path-to/start.el")

(make-temp-file "emacs-" nil ".pid" (format "%d
" (emacs-pid)))

(defvar *log-level* 0); increase it to reduce log size

(mapcar #'require '(cloud ispell))

(cloud-start)
