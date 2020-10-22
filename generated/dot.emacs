:;;; -*- mode: Emacs-Lisp;  lexical-binding: t; -*-

;; generated from cloud.org
(defvar *loaded* (list (file-chase-links "~/.emacs")))
(let ((el-prefix "~/programming/emacs/"))
  (mapcar #'(lambda(x)
              (let ((FN (file-chase-links (concat el-prefix x ".el"))))
                (unless (member FN *loaded*)
                  (push FN *loaded*))
                (load-file FN)))
          '("macros" "functions" "logging" ....)))
