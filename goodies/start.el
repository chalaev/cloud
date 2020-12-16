;; -*- lexical-binding: t; -*-

;; This file is a part of https://github.com/chalaev/lisp-goodies

;; I load this file at startup

(unless (functionp 'caddr) (defun caddr(x) (car(cddr x)))); for emacs versions <26
(unless (functionp 'cadar) (defun cadar(x) (car (cdar x))))

(defun emacs-ver()
  (mapcar #'string-to-number (split-string
   (caddr (split-string (emacs-version))) "\\.")))

(unless (< 25 (car (emacs-ver)))
  (defun upgrade-make-temp-file(old-function PREFIX &optional DIR-FLAG SUFFIX TEXT)
    (let((FN (funcall old-function PREFIX DIR-FLAG SUFFIX)))
      (when (and TEXT (stringp TEXT))
      (write-region TEXT nil FN))
    FN))
(add-function :around (symbol-function 'make-temp-file) #'upgrade-make-temp-file))

;; -*-  lexical-binding: t; -*-
(defvar ~ (file-name-as-directory (getenv "HOME")))
(defun   tilde(x) (replace-regexp-in-string (concat "^" ~) "~/" x))
(defun untilde(x) (replace-regexp-in-string "^~/" ~ x))
(defvar emacs-d (concat "~/" (file-name-as-directory ".emacs.d")))

(require 'package)
(unless (assoc "local-packages" package-archives)
  (push (cons  "local-packages" (concat emacs-d (file-name-as-directory "local-packages")))
	package-archives))
(make-directory (cdr (assoc "local-packages" package-archives)) t)

(unless (member (cdr (assoc "local-packages" package-archives)) load-path)
  (add-to-list 'load-path (cdr (assoc "local-packages" package-archives))))
(require 'shalaev) ; ← needed for perms-from-str unless you have loaded it in another way
(defun after-tangle()
  "mark tangled files as non-backupable (chgrp tmp files) and non-excecutable"
  (let ((FN (buffer-file-name)))

(set-file-modes FN (logand #o666 (perms-from-str (nth 8 (file-attributes FN 'string)))))
    (chgrp "tmp" FN)))
(add-hook 'org-babel-post-tangle-hook #'after-tangle)

(defun printangle(FN)
  "to be used in Makefile instead of org-babel-tangle-file"
  (let ((l (length default-directory)))
    (apply #'concat (mapcar #'(lambda(x) (substring (format "%s " x) l)) (org-babel-tangle-file FN)))))

(defun run-init-block ()
"runs code block labeled 'init' when an org-file is opened in emacs"
  (org-babel-goto-named-src-block "init")
  (org-babel-execute-src-block))
(defun barename (FN)
  (let ((SS (split-string (file-name-nondirectory FN)  "\\." t)))
    (mapconcat #'(lambda(x)x) (butlast SS) ".")))

(let (loaded); prevents duplicate evaluation of files
(defun load* (x &optional el-prefix)
  (let ((FN (tilde (file-chase-links (concat (or el-prefix "~/") x)))))
    (unless (member (car (last (split-string FN "\\." t))) '("el" "elc"))
      (setf FN (concat FN ".el")))
    (unless (member FN loaded) (load-file FN) (push (barename FN) loaded)))))
