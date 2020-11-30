;; -*- mode: Emacs-Lisp;  lexical-binding: t; -*-
;; generated from cloud.org
(defvar password nil); to be read from config or generated
(defvar number-of-CPU-cores 1)
(defvar cloud-file-hooks nil "for special files treatment")

(defvar upload-queue nil "names of edited files")
(defvar added-files nil "newly clouded files")

(defvar remote/files nil "3-symbol DB name on the server, e.g., WzT")
(defvar localhost (system-name))
(defvar ~ (file-name-as-directory(expand-file-name "~")))
(defvar remote-directory  "/mnt/cloud/")

(defvar /tmp/cloud/ (file-name-as-directory (make-temp-file "cloud." t)))
(defun remote/files() remote/files)
(defun remote-directory() remote-directory)
(defun remote-files() (concat (remote-directory) remote/files ".gpg"))
(defun history() (concat (remote-directory) "history"))

(defvar emacs-d "~/.emacs.d/")
(defvar cloud-was-connected t "normally t, nill when there was no connection")

(defun local-dir() (concat emacs-d "cloud/"))
(defun cloud-mk() (concat (local-dir) "cloud.mk"))
(defun lock-dir() (concat (remote-directory) "now-syncing/"))
(defun image-passes() (concat (local-dir) "individual.passes"))
(defun local/() (concat (local-dir) localhost "/"))
(defun local/log() (concat (local/) "log"))

(defun local/config() (concat (local-dir) localhost "/config"))

(defvar numerical-parameters '("number-of-CPU-cores"))
(defvar lists-of-strings '("junk-extensions" "ignored-dirs"))

(defvar cloud-hosts nil "host names participating in file synchronization")
(defvar remote-actions nil "actions to be saved in the cloud")
(defvar file-DB nil "list of vectors, each corresponding to a clouded file")

(defvar *blacklist* '("~/.bash_login" "~/.bash_logout" "~/.bashrc") "list of manually blcklisted files")

(defvar ignored-dirs '("/tmp/" "/mnt/" "/etc/") "temporary or system or remote directories")

(defvar junk-extensions '("ac3" "afm" "aux" "idx" "ilg" "ind" "avi" "bak" "bbl" "blg" "brf" "bst" "bz2" "cache" "chm" "cp" "cps" "dat" "deb" "dvi" "dv" "eps" "fb2"
"fn" "fls" "img" "iso" "gpx" "segments" "ky" "mjpeg" "m" "md" "mov" "mpg" "mkv" "jpg" "gif" "jpeg" "png" "log" "mp3" "mp4" "m2v" "ogg" "ogm" "out" "part" "pbm" "pdf"
"pfb" "pg" "pod" "pgm" "pnm" "ps" "rar" "raw" "gz" "sfd" "woff" "tbz" "tgz" "tga" "tif" "tiff" "toc" "tp" "vob" "vr" "wav" "xcf" "xml" "xz" "Z" "zip")
"files with these extensions will not be *automatically* clouded")

(defvar file-fields; indices numerating array fields
(list 'plain; original (local) file name
'cipher; encrypted file name (base name)
'mtime; modification time
'modes; permissions
'size; file size (should not be saved)
'gname)); group name
(let ((i 0)) (dolist (field-name file-fields) (setf i (1+ (set field-name i)))))

(defun local/all() (concat (local/) "all"))

(defvar removed-files  nil "files that were just removed (or renamed or forgotten) on local host before (cloud-sync)")

(defvar important-msgs nil "these messages will be typically printed at the end of the process")
(defvar gpg-process nil "assyncronous make-process for (en/de)cryption")

(defvar action-fields '(i-time i-ID i-args i-hostnames i-Nargs))
(let ((i 0)) (dolist (AF action-fields) (setf i (1+ (set AF i)))))

(defvar action-IDs '(i-forget i-delete i-rename i-host-add i-host-forget i-share))
(let ((i 0)) (dolist (AI action-IDs) (setf i (1+ (set AI i)))))

(unless (boundp 'DRF) (defvar DRF (indirect-function (symbol-function 'dired-rename-file)) "original dired-rename-file function"))
(unless (boundp 'DDF) (defvar DDF (indirect-function (symbol-function 'dired-delete-file)) "original dired-delete-file function"))
