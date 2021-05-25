;;; cloud.el --- secure cloud storage and syncronization for text files  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Oleg Shalaev <oleg@chalaev.com>

;; Author:     Oleg Shalaev <oleg@chalaev.com>
;; Version:    2.1.0

;; Package-Requires: (cl epg dired-aux timezone diary-lib subr-x shalaev)
;; Keywords:   syncronization, cloud, gpg, encryption
;; URL:        https://github.com/chalaev/cloud

;;; Commentary:

;; This package shares text files between several computers used by one person.
;; For quick start and documentation see
;; https://github.com/chalaev/cloud
  
;;; Code:

;; (mapcar #'require '(cl-lib epg dired-aux timezone diary-lib subr-x shalaev))

(mapcar #'require '(cl-lib dired-aux timezone diary-lib subr-x shalaev nadvice))
;;(require 'el-debug)
(let((localhost (system-name))
file-DB added-files clouded-hosts tobe-uploaded
remote-actions); actions to be saved in the cloud

(let*(
 (local-dir (need-dir *config-directory* "cloud"))
 (local/host/conf (FN local-dir(concat localhost ".conf"))))
(defmacro indices(&rest body)
"saved in a separate file to be used during testing too"
`(let*
((lfs 7)
 (lafs 5)
 (plain 0)
 (uname 1)
 (gname 2)
 (mtime 3)
 (size 4)
 (modes 5)
 (cipher 6)
 (i-time 0)
 (i-ID 1)
 (i-args 2)
 (i-hostnames 3)
 (i-Nargs 4)
 (i-forget 0)
 (i-delete 1)
 (i-rename 2)
 (i-host-add 3)
 (i-host-forget 4)
 (i-share 5))
,@body))
(defmacro pWarn(par-name value)
"displays warning when a parameter is not found in a config file"
`(progn (clog :warning "important parameter %s missing, will use %s" ,par-name ,value)
 (setf configured "no") ,value))

(indices(letc(read-conf-file local/host/conf)
  ((configured "no")
   ((:string) black-root-dirs(split-string "/mnt/ /tmp/ /etc/ /ssh:"))
   ((:string) black-matches (split-string "tmp /old /log /Downloads /.git/"))
   (remote-directory (pWarn "remote-directory" "/mnt/cloud/"))
   (remote/files (pWarn "remote/files" (rand-str 3))); "3-symbol DB name on the server, e.g., WzT"
   (:integer number-of-CPU-cores (CPU-cores))
   (password (pWarn "password" (rand-str 8)))
((:string) black-extensions (split-string "aux idx ilg ind bak bbl blg brf bst dvi log out ps wav")))
;;   test>;;(debug-set* remote-directory remote/files password)
(clog :debug "1 local/host/conf= %s" local/host/conf)
(clog :debug "1 configured= %s" configured)
(unless(file-exists-p remote-directory) (clog :error "remote-directory %s specified in %s does not exist; examine/update %s before proceeding" remote-directory local/host/conf local/host/conf))

(let*((local-dir   (need-dir *config-directory* "cloud"))
      (local/host/ (need-dir local-dir localhost))
 uploaded cloud-file-hooks all Makefile stanze

upload-queue added-files; names of edited files and newly clouded files.

file-blacklist

(/tmp/cloud/ (need-dir(make-temp-file "cloud." t)))
(cloud-was-connected t); normally t, nill when there was no connection

;;(defun /tmp/cloud/() (need-dir /tmp/cloud/))
(contents-FN (FN remote-directory (concat remote/files ".gpg")))
(history (FN remote-directory "history"))
(cloud-mk (tilde(concat local-dir "cloud.mk")))
(lock-dir (to-dir remote-directory "now-syncing"))
(image-passes (tilde(FN local-dir "individual.passes")))
(local/log (concat local/host/ "log"))

(local/all (concat local/host/ "all"))
removed-files; files that were just removed (or renamed or forgotten) on local host before (cloud-sync)

important-msgs; these messages will be typically printed at the end of the process
  gpg-process; assyncronous make-process for (en/de)cryption
  (pass-d  (to-dir local-dir "pass.d"))
  (updated (concat pass-d "updated")))
(setf remote-directory (file-name-as-directory remote-directory))
(require 'timezone)
;; specifying Moscow time zone (do this for other important unspecified time zones)
(unless (assoc "MSK" timezone-world-timezones)
  (push '("MSK". 300) timezone-world-timezones))
(unless (assoc "MSD" timezone-world-timezones)
  (push '("MSD". 400) timezone-world-timezones))

(defun parse-time(str)
  (let* ((TPD (timezone-parse-date str)) (TZ (aref TPD 4)))
    (unless (assoc TZ timezone-world-timezones)
      (clog :error "Unknown time zone abbreviation %s, update 'timezone-world-timezones' variable" TZ))
    (apply #'encode-time (append
			  (firstN
			   (parse-time-string
			    (format "%s-%s-%s %s" (aref TPD 0) (aref TPD 1) (aref TPD 2) (aref TPD 3)))
			   8)
			  (list (* 60 (timezone-zone-to-minute TZ)))))))

(defun full-TS (time)
  (format-time-string "%F %H:%M:%S %Z" time))

(defun TS (time)
  (format-time-string "%02m/%02d %H:%M:%S" time))
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
;; -*-  lexical-binding: t; -*-
(defun get-file-properties* (FN)
(when FN
  (or (cloud-locate-FN FN) (cloud-get-file-properties(file-chase-links FN)))))

(defun cip-ext (FN)
"extension of encrypted file based on the original name"
(case* (file-name-extension FN) string=
       ("jpeg" ".png")
       ("jpg" ".png")
       ("png" ".png")
       (otherwise ".gpg")))

(defun forget-password(XYZ)
  "removes image password from password file"
(let* ((str (progn
	     (find-file image-passes)
	     (buffer-string)))
       (BN (buffer-name)))
  (with-temp-file image-passes
    (insert (replace-regexp-in-string (format "%s .*
" XYZ) "" str)))
  (kill-buffer BN)))

(defmacro bad-column (cType N &optional str)
(if str
`(clog :error "invalid %dth column in %s line = %s" ,N ,cType ,str)
`(clog :error "invalid %dth column in %s line" ,N ,cType)))

(defun gpg-encrypt(FN XYZ)
(= 0 (shell-command
  (format "gpg --batch --yes --pinentry-mode loopback --passphrase %S -o %s --symmetric %s" password (FN remote-directory (concat XYZ ".gpg")) (untilde FN)))))

(defun gpg-decrypt(FN XYZ)
(= 0 (shell-command 
(format "gpg --batch --yes --pinentry-mode loopback --passphrase %S -o %s --decrypt %s" password (untilde FN) (FN remote-directory (concat XYZ ".gpg"))))))

(defun replace-file-ext(FN new-ext)
  "replacing file extension"
  (concat (file-name-sans-extension FN) "." new-ext))

(defun youngest(&rest FNs)
  (car (sort FNs #'file-newer-than-file-p)))
(cl-macrolet((cloud-NL() '(push "
" Makefile))
(inl(&rest format-pars) `(progn (push (format ,@format-pars) Makefile) (cloud-NL)))
(h(FN) `(untilde(tilde ,FN) "$(HD)")))
(defun reset-Makefile()
"reseting make file"
(when (or (and (file-exists-p pass-d) (file-directory-p pass-d)) (ensure-dir-exists pass-d))
(setf stanze nil Makefile nil uploaded nil)
(inl "HD=%s
# ← home directory" (file-name-as-directory ~))
(inl "cloud=%s
# ← remote directory" remote-directory)
(inl "password=%S" password)
(inl "gpg=gpg --pinentry-mode loopback --batch --yes")
(inl "enc=$(gpg) --symmetric --passphrase $(password) -o")
(inl "dec=$(gpg) --decrypt   --passphrase $(password) -o")
(inl "localLog=%s
# ← log file" (untilde(tilde  local/log) "$(HD)"))
(inl "MK=%s" (untilde cloud-mk "$(HD)"))
(inl "date=`date '+%%m/%%d %%T'`
")

(inl "%%/:
\t[ -d $@ ] || mkdir -p $@
")

(inl (concat (format "%s: %s
\tawk '{print $$2 > %S$$1}' $<
\techo $(date) > $@
\t-chgrp -R tmp %s*
" (h updated) (h image-passes) (h pass-d) (h pass-d))))))
(clog :debug "2 configured= %s" configured)
(unless(string= "yes" configured)
  (clog :warning "Unconfigured system: %s is either invalid or non-existent" local/host/conf)

(when(and(file-exists-p remote-directory) (directory-files remote-directory nil "^...\\.gpg$" t))
    (clog :warning "I am not the first one who uses remote directory %s " remote-directory))
(push-new clouded-hosts localhost)

(if(file-exists-p local/host/conf) (clog :warning "will not overwrite existing %s" local/host/conf)
  (clog :info "creating new config file %s" local/host/conf)
  (clog :debug "3 configured= %s" configured)
  (write-conf local/host/conf 
       (make-conf password configured black-matches black-root-dirs remote-directory remote/files black-extensions number-of-CPU-cores)))

(ifn(need-dir remote-directory)
 (clog :error "could not create remote directory %s" remote-directory)
(clog :warning "please examine your config file %s" local/host/conf))

(reset-Makefile))

(defun print-hosts()
  (push-new clouded-hosts localhost)
  (dolist (hostname(delete-dups clouded-hosts)) (insert (format "%s " hostname)))
  (backspace)
  (newline))

(defun print-actions()
(dolist (action remote-actions)
  (insert (format-action action))
  (drop remote-actions action)
(newline)))

(defun format-file (DB-rec)
  (format "%S %s %s %s %d %S"
    (tilde (aref DB-rec plain))
    (aref DB-rec cipher)
    (aref DB-rec size)
    (aref DB-rec gname)
    (aref DB-rec modes); integer
    (format-time-string "%F %H:%M:%S %Z" (aref DB-rec mtime))))

(defun parse-action(str)
(let((action (make-vector lafs nil)))

(dolist (column (list
                 `(:time-stamp . ,i-time)
                 `(:int . ,i-ID)
                 `(:int . ,i-Nargs)))
  (needs((col-value (begins-with str (car column)) (bad-column "action" (cdr column))))
     (aset action (cdr column) (car col-value))
     (setf str (cdr col-value))))

(dolist (column (list
  (cons (cons :string (aref action i-Nargs)) i-args); e.g., → ((:string . 1) . 2)
  `(:others . ,i-hostnames)))
  (needs((col-value(begins-with str (car column)) (bad-column "action" (cdr column)))); → (("hostB") . " hostB hostA")
     (aset action (cdr column) (car col-value)); → 
     (setf str (cdr col-value))))

(let((AID (format-time-string "%02m/%02d %H:%M:%S" (aref action i-time))))
(clog :info "... will later be referenced as %s" AID)
(cons AID action))))

(defun str-to-DBrec(str)
  "parses one file line from the remote file DB"
(ifn (string-match "\"\\(.+\\)\"\s+\\([^\s]+\\)\s+\\([^\s]+\\)\s+\\([^\s]+\\)\s+\\([[:digit:]]+\\)\s+\"\\(.+\\)\"" str)
  (clog :error "Ignoring invalid file line %s" str)

(let((CF (make-vector lfs nil))
     (FN (tilde(match-string 1 str))))
  (aset CF plain FN)
  (aset CF cipher (match-string 2 str))
  (aset CF size (string-to-number (match-string 3 str)))

(aset CF gname (match-string 4 str))
  (aset CF modes (string-to-number (match-string 5 str)))
  (let ((mtime-str (match-string 6 str)))
(ifn (string-match "[0-9]\\{4\\}-[0-9][0-9]-[0-9][0-9] [0-9][0-9]:[0-9][0-9]:[0-9][0-9] [[:upper:]]\\{3\\}" mtime-str)
(bad-column "file" 6 mtime-str)
(aset CF mtime (parse-time mtime-str))
CF)))))

(defun end-log (fstr &rest args)
  "message + time"
  (push
   (apply #'format
	  (cons (concat
		 (format-time-string "%H:%M:%S " (apply 'encode-time (butlast (decode-time (current-time)) 3)))
		 fstr)
		args))
   important-msgs))

(defun post-decrypt (FN)
  "special treatment for certain files"
  (let ((ext (file-name-extension FN))
	(name (file-name-base FN)))
    (when (string= FN (expand-file-name diary-file))
      (with-current-buffer (find-file-noselect (diary-check-diary-file))
	(clog :info "diary buffer opened or updated")))
     (when (member FN *loaded*)
       (end-log "*configuration changed, consider reloading emacs*")
    (clog :warning "consider reloading configuration file %s" FN))))

(defun cloud-connected-p()
   (file-readable-p remote-directory))

(defun write-all(DBname)
  (with-temp-file DBname
(print-hosts)

(print-actions)

(newline)

(dolist (file-record file-DB)
  (insert (format-file file-record)) (newline))
(setf removed-files nil) t))

(defun read-all(DBname)
  "reads content (text) file into the database file-DB"
(with-temp-buffer (insert-file-contents DBname)
  (let((str(read-line)))
(needs-set((clouded-hosts (split-string str)
  (clog :error "(o.k. if this is the very first run) Invalid first line in the remote file DB %s" DBname)))
(unless (member localhost clouded-hosts) (cloud-host-add))

(while (< 0 (length (setf str (read-line))))
(clog :debug "read-all> action string= %s" str); "2021-05-03 19:39:22 EDT" 2 2 "~/file-1a.dat" "~/new-file-1a.dat"  hostB
(when-let((AA(parse-action str)) (AID(car AA)) (action(cdr AA))); e.g., AID="05/03 16:59:36"
(clog :debug "read-all> format(parsed action)= %s" (format-action action)); "2021-05-03 19:39:22 EDT" 2 2 "~/file-1a.dat" "~/new-file-1a.dat"
  (ifn(member localhost (aref action i-hostnames))
      (clog :info "this host (= %s) is unaffected by action %s (= %s)" localhost AID (format-action action))
    (if (perform action (aref action i-hostnames))
	(clog :info "sucessfully performed action %s" AID)
      (clog :error " action %s failed, will NOT retry it" AID))

(when (drop (aref action i-hostnames) localhost)
  (end-push action remote-actions)))))

(needs((CDFs

(mapcar #'(lambda(s) (replace-regexp-in-string "\\.gpg$" "" s))
      (directory-files remote-directory nil "^...\\.gpg" t)) 
(clog :warning "did not find any gpg-files in %s; is this the very first run?" remote-directory)))
(while(< 10 (length (setf str (read-line))))
(when-let((CF(str-to-DBrec str)))

(let* ((FN (untilde(plain-name CF))); ~/file-1.qieFmS
       (CN (aref CF cipher))
       (remote-file-exists (member CN CDFs)); t
       (local-file-rec (or 
(cloud-locate-FN FN); either this file is already clouded
(and (not (member FN removed-files))
     (when-let ((LF (cloud-get-file-properties FN)))
        (aset LF cipher (aref CF cipher)); or it exists
        (push LF file-DB); but unclouded
        LF)))))
(cond

((not (or local-file-rec remote-file-exists))
 (clog :error "forgetting file %s which is marked as clouded but is neither on local disk nor in the cloud" FN)
 (drop file-DB CF))

((or
 (and (not local-file-rec) remote-file-exists)
 (and local-file-rec remote-file-exists (time< (aref local-file-rec mtime) (aref CF mtime))))

(if local-file-rec
   (aset local-file-rec mtime (aref CF mtime))
   (push CF file-DB))
(download CF))
((or
 (and local-file-rec remote-file-exists (time< (aref CF mtime) (aref local-file-rec mtime)))
 (and local-file-rec (not remote-file-exists)))
(when (and local-file-rec remote-file-exists)
  (clog :debug "read-all/upload: local %s(%s) is younger than %s.gpg(%s)"
  (aref local-file-rec plain) (TS(aref local-file-rec mtime))
  (aref CF cipher) (TS(aref CF mtime)))
(push CF tobe-uploaded)))))))
t))))); end of read-all

(defun cloud-touch(&rest FNs)
"called when the files named FNs are changed"
  (interactive)
(dolist(FN FNs)
(let((FR(cloud-locate-FN FN)))
(unless FR
  (auto-add-file FN)
  (setf FR (cloud-locate-FN FN)))
(when FR
    (aset FR mtime (current-time))
    (clog :debug "touch/upload: %s(%s)" FN (TS(aref FR mtime)))
    (push FR tobe-uploaded)))))
(defun on-current-buffer-save()
  (when-let ((FN (buffer-file-name)))
    (cloud-touch FN)))
(add-hook 'after-save-hook 'on-current-buffer-save)

(defun enc-make-stanza(file-record)
  (when-let((XYZ (aref file-record cipher)) (FN (h(aref file-record plain))))

(let ((file-ext(file-name-extension FN)))
(concat (cond

((member file-ext '("gz" "tgz"))
(let((gunzipped (make-temp-file "emacs-cloud.")))
(format "
%s: %s
\tzcat $< > $@

$(cloud)%s.gpg: %s
\t@$(enc) $@ $<
\trm $<
" gunzipped FN XYZ gunzipped)))

((member file-ext '("bz2" "tbz"))
(let ((gunzipped (make-temp-file "emacs-cloud.")))
(format "
%s: %s
\tbzcat $< > $@

$(cloud)%s.gpg: %s
\t@$(enc) $@ $<
\trm $<
" gunzipped FN XYZ gunzipped)))

((string= "gpg" file-ext)
(format "
$(cloud)%s.gpg: %s
\tcp $< $@
" XYZ FN))

((member file-ext '("jpg" "jpeg" "png"))
(format "
$(cloud)%s.png: %s %s
\tconvert $< -encipher %s%s $@
"
XYZ FN (h updated)
(h pass-d) XYZ))

(t 
(format "
$(cloud)%s.gpg: %s
\t@$(enc) $@ $<
" XYZ FN)))

"\t-@echo \"$$(date): uploaded $<\" >> $(localLog)
"))))

(defun dec-make-stanza(file-record)
  (when-let((XYZ(aref file-record cipher)) (FN0(aref file-record plain)))
    (let((file-ext(file-name-extension FN0)) (DN(file-name-directory(untilde FN0))) (FN(h FN0)))
(concat (cond

((string= "gpg" file-ext)
(format "
%s: $(cloud)%s.gpg %s
\tcp $< $@
" FN XYZ DN))

((member file-ext '("jpg" "jpeg" "png"))
(format "
%s: $(cloud)%s.png %s %s
\tconvert $< -decipher %s%s $@
"
FN XYZ (h updated) DN
(h pass-d) XYZ))

((member file-ext '("gz" "tgz"))
(let((gunzipped (make-temp-file "emacs-cloud.")))
  (format "
%s: $(cloud)%s.gpg %s
\t@$(dec) $@ $<

%s: %s
\tcat $< | gzip > $@
\trm $<
" 
gunzipped XYZ DN
FN gunzipped)))

((member file-ext '("bz2" "tbz"))
(let((gunzipped (make-temp-file "emacs-cloud.")))
  (format "
%s: $(cloud)%s.gpg %s
\t@$(dec) $@ $<

%s: %s
\tcat $< | bzip2 > $@
\trm $<
" 
gunzipped XYZ DN
FN gunzipped)))

(t (format "
%s: $(cloud)%s.gpg %s
\t@$(dec) $@ $<
" FN XYZ DN)))

(format "\t-chgrp %s $@
\t-chmod %o $@
\t-touch --date=%S $@
\t-@echo \"$$(date): downloaded $@\" >> $(localLog)
"
(aref file-record gname) (aref file-record modes) (full-TS (aref file-record mtime)))))))

(defun download(FR)
(needs ((FN (aref FR plain) (clog :error "download: file lacks plain name"))
        (stanza (dec-make-stanza FR) (clog :error "download: could not create stanza for %s" FN)))
(let((DN (file-name-directory FN)))
  (condition-case err
      (progn
	(ensure-dir-exists DN)
	(push FN stanze)
	(push stanza Makefile) (cloud-NL))
    (file-error
     (clog :error "failed to download %s: could not create %s: %s" FN DN (error-message-string err)))))))

(defun make-cloud-older(FR)
(when-let ((FN (aref FR plain))
           (RN (FN remote-directory (concat (aref FR cipher) (cip-ext FN))))
           (clouded (cloud-get-file-properties RN))
           (local-mtime (aref FR mtime)))
(clog :debug "make-cloud-older: FN= %s, RN= %s" (tilde FN) RN)
(when (time< local-mtime (aref clouded mtime))
  (set-file-times RN
(time-add local-mtime (- -60 (random 6000)))))))

(defun upload(FR)
"creating stnze with uploaded files"
(needs ((FN (tilde(aref FR plain)) (clog :error "upload: file lacks plain name"))
	(CN (aref FR cipher) (clog :error "upload: file %s lacks cipher name" FN))
	(stanza (enc-make-stanza FR) (clog :error "upload: could not create stanza for %s" FN)))
  (unless (or (member FN uploaded) (member FN file-blacklist))
    (push FN upload-queue)
    (make-cloud-older FR)
    (push FN uploaded)
    (push (format " %s" (FN remote-directory (concat CN (cip-ext FN)))) stanze)
    (push stanza Makefile) (cloud-NL))))

(defun save-Makefile()
"flushing make file"
(while tobe-uploaded (upload (pop tobe-uploaded)))
(inl "all:%s
\techo \"background (en/de)cryption on %s finished $(date)\" >> %s
\t@sed 's/%s/******/g' %s > %s.bak
"
(together stanze)
localhost
history
password (h cloud-mk) (h cloud-mk))
(write-region (apply #'concat (reverse Makefile)) nil (untilde cloud-mk))

(setf stanze nil added-files nil upload-queue nil removed-files nil)
(reset-Makefile)))

(defun cloud-sync()
(interactive) (error-in "cloud-sync"

(cl-flet((do-make()
  (set-file-times local/all (current-time)); touch local file DB
  (save-Makefile)
(let((make (format "HOME=%s make -j%d -ikf %s all &> %s.log" (directory-file-name ~) number-of-CPU-cores (untilde cloud-mk) (untilde cloud-mk))))

(clog :info "make started on %s" (format-time-string "%H:%M:%S.%3N" (current-time)))
(ifn(= 0 (shell-command make)) (clog :error "make file %s containing

%s
FAILED with error(s): %s" (untilde cloud-mk) (cat-file(untilde cloud-mk)) (cat-file(concat(untilde cloud-mk)".log")))
(delete-file(untilde cloud-mk)))
(clog :info "make finished on %s" (format-time-string "%H:%M:%S.%3N" (current-time))))))

(ifn(cloud-connected-p) (clog :warning "refuse to sync because remote directory not mounted")
(directory-lock lock-dir (format "%s
%s" localhost (TS(current-time)))
;; (debug-log-var contents-FN) (debug-log-var local/all)

(ifn (or (file-exists-p contents-FN) (file-exists-p local/all))
(progn(clog :info "first run: creating %s and %s" local/all remote/files)
(reset-Makefile)
(ifn (write-all local/all) (clog :error "could not save data to %s" local/all)
(ifn (gpg-encrypt local/all remote/files) (error "could not encrypt %s to %s" local/all remote/files)
(do-make))))

(when(file-newer-than-file-p contents-FN local/all)
(clog :debug "updating %s obsoleted by %s" local/all contents-FN)
(ifn(gpg-decrypt local/all remote/files) (error "could not DECRYPT file data FROM the cloud")
(read-all local/all)))

(when (or added-files upload-queue removed-files remote-actions)
  (ifn(write-all local/all) (error "could not save data to %s" local/all)
    (unless(gpg-encrypt local/all remote/files) (error "could not ENCRYPT %s TO the cloud(%s)" local/all remote/files))))

(do-make)))

(dolist (msg(reverse important-msgs)) (message msg))
(setf important-msgs nil)
(clog :info "done syncing")
(write-region (format "%s: %s -- %s
" localhost (TS (current-time)) (format-time-string "%H:%M:%S" (current-time))) nil history t)))))

(defun before-exit()
  (when (cloud-sync) (delete-directory /tmp/cloud/)))

(defun new-action(a-ID &rest args)
  (let((action (make-vector lafs nil)))
    (aset action i-ID a-ID)
    (aset action i-time (current-time))
    (aset action i-args args)
    (aset action i-hostnames (remove localhost clouded-hosts))
    (end-push action remote-actions)
(clog :debug "new-action> %s" (format-action action))))

(defun perform(action &optional HNs)
"performing an action locally"
(write-region
(format "%s: %s
" (TS (current-time)) (format-action action))
nil local/log t)
  (let ((arguments (aref action i-args)))
    (case* (aref action i-ID) =
      (i-host-forget (dolist (arg arguments) (drop clouded-hosts arg)) t)
      (i-host-add (dolist (arg arguments) (push-new clouded-hosts arg)) t)
      (i-forget (cloud-forget-many arguments) t)
      (i-delete (cloud-rm arguments) t)
      (i-rename (dired-rename-file (untilde(car arguments)) (untilde(cadr arguments)) t)); 05/25

(i-share (when (= 1 (length HNs)) (cloud-forget-many arguments)))
(otherwise (clog :error "unknown action %d" (aref action i-ID))))))

(defun format-action(action)
  (format "%S %d %d %s %s"
(full-TS (aref action i-time)); 1. Time stamp,
(aref action i-ID); 2. (integer) action ID,
(length (aref action i-args)); 3. (integer) number of arguments for this action (one column),
(apply #'concat (mapcar #'(lambda(arg) (format "%S " (tilde arg))) (aref action i-args))); 4. [arguments+] (several columns),
(together (aref action i-hostnames)))); 5. hostnames, where the action has to be performed (several columns).

;; (require 'nadvice)
;; (advice-remove #'dired-delete-file 'dired-delete-file@DDF)
(define-advice dired-delete-file (:after (FN &optional RECURSIVE TRASH) DDF)
   (cloud-forget FN)
   (unless(BRDp FN) (new-action i-delete FN)))

(defun cloud-rm(args)
(cloud-forget-many args)
(error-in "cloud-rm"
(dolist (arg args)
  (ifn(file-directory-p arg)(delete-file arg)
  (delete-directory arg t)
  (cloud-forget-recursive arg)))))

(defun cloud-forget-many (args)
  (error-in "cloud-forget-many"
    (dolist (arg args)
      (unless(cloud-forget-recursive arg) (error "could not forget %s" arg)))))

(defun contained-in(DN)
  (let* ((dir-name (tilde DN)) res (dir-name (to-dir dir-name)))
    (dolist (DB-rec file-DB)
      (let((FN(tilde(aref DB-rec plain))))
        (when(and (< (length dir-name) (length FN))
                  (string=(substring-no-properties FN 0 (length dir-name)) dir-name))
          (push DB-rec res))))
      res))

(defun add-to-actions(hostname)
  (dolist (action remote-actions)
    (unless (member hostname (aref action i-hostnames))
      (aset action i-hostnames (cons hostname (aref action i-hostnames))))))
(defun erase-from-actions(hostname)
  (dolist (action remote-actions)
    (when (member hostname (aref action i-hostnames))
      (aset action i-hostnames (remove hostname (aref action i-hostnames))))))

(defun cloud-host-add ()
  "adding THIS host to the cloud sync-system"
  (unless (member localhost clouded-hosts)
    (push-new clouded-hosts localhost))
  (new-action i-host-add localhost)
  (add-to-actions localhost))

(defun cloud-host-forget()
  "remove host from the cloud sync-system"
    (when (yes-or-no-p (format "Forget the host %s?" localhost))
      (new-action i-host-forget localhost)
      (if(cloud-sync)
        (dired-delete-file  local/host/conf "always")
	(clog :error "sync failed, so I will not erase local configuration"))))

(defun cloud-add(&optional FN)
(interactive)
(if FN (add-file FN)
  (if (string= major-mode "dired-mode")
      (dired-map-over-marks (add-file (dired-get-filename)) nil)
    (if-let ((FN (buffer-file-name))) (add-file FN)
    (unless
	(add-file (read-string "file to be clouded=" (if FN FN "")))
      (clog :error "could not cloud this file"))))))

(defun blacklist(FN)
(when FN
(let((FN (tilde FN)))
 (cloud-forget-file FN)
(unless (member FN file-blacklist)
 (push FN file-blacklist)))))
(defun BRDp(FN)
  (when black-root-dirs (string-match (eval `(rx bol ,(cons 'or black-root-dirs))) FN)))
(defun black-p(FN &optional file-rec)
(let*((result
(or
 (member FN file-blacklist)
 (when black-matches (string-match (eval `(rx ,(cons 'or black-matches))) FN))
 (string-match (concat ~ "\\.") (untilde FN))
 (member (file-name-extension FN) black-extensions)
 (backup-file-name-p FN)
 (BRDp FN)
 (progn
   (unless file-rec (setf file-rec (get-file-properties* FN)))
   (when file-rec
     (or
      (member (aref file-rec gname) '("tmp"))
      (< 1048000 (aref file-rec size))))))))
  (cons result file-rec)))

(defun white-p(FN &optional FR)
(when FN
  (unless FR (setf FR (get-file-properties* FN)))
  (cons (member (aref FR gname) '("important" "keepOneYear" "keepTwoYears" "keepThreeYears")) FR)))

(defun add-file(FN &optional file-rec)
(when FN
(let((FN (untilde (file-chase-links FN))))
(unless (cloud-locate-FN FN)
(ifn(file-directory-p FN)
  (needs ((GFP (or file-rec (cloud-get-file-properties FN)) (clog :error "cannot cloud inexisting file %s" FN))
          (CN (new-file-in remote-directory)) (FN (tilde FN)))
    (push FN added-files)
    (aset GFP cipher CN)
    (push GFP file-DB) (clog :info "file %s is now clouded" FN)
    (push GFP tobe-uploaded)
    (when (member (file-name-extension FN) '("jpeg" "png" "jpg"))

(write-region
  (format "%s %s
" CN (rand-str 18)) nil  (untilde image-passes) t)
(cloud-touch image-passes)))

(let ((DN (to-dir FN)))
(dolist (FN (directory-files DN nil nil t))
(unless (member FN '("." ".."))
(let ((FN (concat DN FN)) FR)

(if (or
(let ((r (white-p FN))) (setf FR (cdr r)) (car r))
(not
(let ((r (black-p FN FR))) (setf FR (cdr r)) (car r))))
(add-file FN FR)
;; (clog :debug "not auto-clouding %s" FN)
))))))))))

(defun auto-add-file(FN &optional file-rec)
"when the file is clouded automatically"
 (unless (car(black-p FN file-rec)) (add-file FN file-rec)))

(defun cloud-forget-file(FN)
  (needs((DB-rec(cloud-locate-FN FN) (clog :warning "forget: doing nothing since %s is not clouded" FN))
         (CN(aref DB-rec cipher))
         (CEXT (cip-ext FN))
	 (cloud-FN (FN remote-directory (concat CN CEXT))))
(drop stanze (tilde FN) (untilde FN)); cacelling pending upload

(when (string= CEXT ".png")
  (forget-password CN))

(drop file-DB DB-rec)
(push FN removed-files)
(dired-delete-file cloud-FN "always")))

(defun cloud-forget-recursive(FN)
(clog :debug "cloud-forget-recursive> FN= %s" FN)
(new-action i-forget FN)
(dolist (sub-FN (mapcar #'plain-name (contained-in FN)))
  (clog :debug "cloud-forget-recursive> sub-FN= %s" sub-FN)
  (cloud-forget-file sub-FN))
(unless(file-directory-p FN) (cloud-forget-file FN)))

(defun cloud-forget (&optional FN)
  (interactive)
(if FN (cloud-forget-recursive FN)
  (if (string= major-mode "dired-mode")
      (dired-map-over-marks(cloud-forget-recursive(dired-get-filename)) nil)
(if-let((FN (buffer-file-name))) (cloud-forget-recursive FN)
    (unless
	(cloud-forget-recursive (read-string "file to be forgotten=" (if FN FN "")))
      (clog :error "could not forget this file"))))))

(defun cloud-rename-file(old new)
  (let ((source (cloud-locate-FN old))
        (target (cloud-locate-FN new)))
    (cond
     ((and source target); overwriting one cloud file with another one
        (dolist (property (list mtime modes gname))
            (aset target property (aref source property)))
        (drop file-DB source))
     (source
 (aset source plain new)
)
     (target (setf target (cloud-get-file-properties new))))
(when(file-exists-p old)
  (unless(file-exists-p(file-name-directory new)) (make-directory(file-name-directory new))); ← do we really need this?
  (error-in "cloud-rename-file" (rename-file old new t) t))))

(defun DRF(old-function old-FN new-FN ok-if-already-exists)
"always called with dired-rename-file"
(if(BRDp old-FN); 05/25 got doubts about this block
 (clog :debug "(BRDp %s) is t!" old-FN)
 (new-action i-rename old-FN new-FN))
(let((is-Dir (file-directory-p old-FN))); Now let us finally rename the file (or directory) →
(error-in "DRF" (funcall old-function (untilde old-FN) (untilde new-FN) ok-if-already-exists))
(ifn is-Dir
  (cloud-rename-file old-FN new-FN); cloud-rename-file is for files only

(let* ((old-dir (to-dir old-FN)) (LOD (length old-dir))
       (new-dir (to-dir new-FN)))
  (dolist (rec(contained-in old-FN))
    (let((FN(aref rec plain)))
      (when (and (<= LOD (length FN))
                 (string= old-FN (substring FN 0 (1- LOD))))
	  (let((new-name(concat new-dir (substring FN LOD))))
            (cloud-rename-file FN new-name)))))))))
(advice-add 'dired-rename-file :around #'DRF)

(defun cloud-start()
(clog :debug "4 configured= %s" configured)
(ifn(string= "yes" configured) (clog :error "Refusing to start because something is wrong with the config file %s" local/host/conf)
(save-some-buffers)
(unless(file-exists-p(untilde image-passes))
  (write-region "" nil(untilde image-passes)))
(add-file (untilde image-passes)); does nothing except for the very first time

(ifn remote-directory (clog :error "You have to set remote-directory for me before I can proceed")
(ifn password (clog :error "You have to set encryption password for me before I can proceed")

(add-hook 'kill-emacs-hook 'before-exit)
(reset-Makefile)
(when(file-exists-p local/all) (read-all local/all))
(cloud-sync))))))))))
(provide 'cloud)
;; cloud.el ends here
