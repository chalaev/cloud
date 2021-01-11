;; -*- mode: Emacs-Lisp;  lexical-binding: t; -*-
(defvar file-fields; indices numerating array fields
(list

'plain; original (local) file name
'uname; (not used in this project) user name
'gname; group name
'mtime; modification time
'size; file size (should not be saved)
'modes; permissions
'cipher)); (a non-standard field) pseudonim used in remote (cloud) directory
(let((i 0))
 (dolist (field-name file-fields) (setf i (1+ (set field-name i)))))

(define-vars (password; to be read from config or generated
(HOME (getenv "HOME")); must not have trailing "/"
(number-of-CPU-cores 1)
(black-root-dirs(split-string "/mnt/ /tmp/ /etc/ /ssh:"))
(black-matches (split-string "tmp /old /log /Downloads /.git/"))
cloud-file-hooks; "for special files treatment"

upload-queue; "names of edited files"
added-files; "newly clouded files"

remote/files; "3-symbol DB name on the server, e.g., WzT"
(localhost (system-name))
(~ (to-dir(expand-file-name "~")))
(remote-directory  "/mnt/cloud/")

(/tmp/cloud/ (need-dir (make-temp-file "cloud." t)))))
(defun /tmp/cloud/() (need-dir /tmp/cloud/))
(defun remote/files() remote/files)
(defun remote-directory() remote-directory)
(defun remote-files() (concat (remote-directory) remote/files ".gpg"))
(defun history() (concat (remote-directory) "history"))

;; (defvar emacs-d "~/.emacs.d/") defined in shalaev.el
(define-vars ((cloud-was-connected t))); normally t, nill when there was no connection

(defun local/host/conf() (concat (local/host/) "config"))

(define-vars ((numerical-parameters '("number-of-CPU-cores"))
              (lists-of-strings (split-string "black-extensions black-root-dirs black-matches"))))

(define-vars (cloud-hosts; host names participating in file synchronization
remote-actions; actions to be saved in the cloud
file-DB; list of vectors, each corresponding to a clouded file

file-blacklist

(black-extensions(split-string
"ac3 afm aux idx ilg ind avi bak bbl blg brf bst bz2 cache chm cp cps dat deb dvi dv eps fb2 fn fls img iso gpx segments ky mjpeg m md mov mpg mkv jpg gif jpeg png log mp3 mp4 m2v ogg ogm out part pbm pdf pfb pg pod pgm pnm ps rar raw gz sfd woff tbz tgz tga tif tiff toc tp vob vr wav xcf xml xz Z zip"))))

(defun local/all() (concat (local/) "all"))

(defvar removed-files  nil "files that were just removed (or renamed or forgotten) on local host before (cloud-sync)")

(defvar important-msgs nil "these messages will be typically printed at the end of the process")
(defvar gpg-process nil "assyncronous make-process for (en/de)cryption")

(defvar action-fields '(i-time i-ID i-args i-hostnames i-Nargs))
(let ((i 0)) (dolist (AF action-fields) (setf i (1+ (set AF i)))))

(defvar action-IDs '(i-forget i-delete i-rename i-host-add i-host-forget i-share))
(let ((i 0)) (dolist (AI action-IDs) (setf i (1+ (set AI i)))))
