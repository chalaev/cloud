;; -*-  mode: Emacs-Lisp; lexical-binding: t; -*-
(defun local-dir() (need-dir emacs-d "cloud"))
(defun local/host/() (need-dir (local-dir) localhost))
(defun cloud-mk() (tilde(concat (local-dir) "cloud.mk")))
(defun lock-dir() (to-dir (remote-directory) "now-syncing"))
(defun image-passes() (concat (local-dir) "individual.passes"))
(defun local/() (need-dir (local-dir) localhost))
(defun local/log() (concat (local/) "log"))

(defun cloud-init(&optional rem-dir) 
"initializes cloud directory and generates password -- runs only once"
(let ((remote-directory (ensure-dir-exists (or rem-dir remote-directory))))

(if (directory-files remote-directory nil "^.\+.gpg$" t)
    (clog :error "please clean the directory %s before asking me to initialize it" remote-directory)
(clog :info "will use (remote) unused directory %s as a cloud" remote-directory)
(ifn-set ((remote/files (new-file-name remote-directory)))
  (clog :error "could not create DB file in %s" remote-directory)

(unless password (setf password (rand-str 9)))
(reset-Makefile)

(ensure-dir-exists (local-dir)) (write-conf)
(clog :info "saved local configuration in %s" (local-dir))))))

(defun format-conf(CP)
(cond
  ((member CP numerical-parameters) (format "%s=%d" CP (symbol-value(intern CP))))
  ((member CP lists-of-strings) (format "%s=%s" CP
(apply #'concat (mapcar #'(lambda(item) (format "%s " item)) (sort (symbol-value(intern CP)) #'string<)))))
  (t (format "%s=%s" CP (symbol-value(intern CP))))))

(defun write-conf()
;;(clog :debug "starting write-conf")
(with-temp-file (local/host/conf)
(mapcar #'(lambda(CP) (insert(format-conf CP)) (newline)) 
  '("remote-directory" "black-extensions" "black-root-dirs" "remote/files" "number-of-CPU-cores" "password")))
;;(clog :debug "ended write-conf")
 t)

(defun read-conf()
  "reads configuration file"
(let ((conf (read-conf-file (local/host/conf))))
(ifn conf (clog :error "refuse to work until you specify 3-symbol contents name \"remote/files\" in %s" (local/host/conf))
(dolist (CP (mapcar #'car conf))
;;(clog :debug "read-conf(%s)" CP)
  (setcdr (assoc CP conf)
    (cond
((member CP numerical-parameters) (string-to-number (cdr (assoc CP conf))))
((member CP lists-of-strings)  (split-string (cdr (assoc CP conf))))
(t (car (split-string (cdr (assoc CP conf))))))))
conf)))

(defun print-hosts()
(unless cloud-hosts (push localhost cloud-hosts))
  (dolist (hostname cloud-hosts) (insert (format "%s " hostname)))
  (backspace)
  (newline))

(defun print-actions()
(dolist (action remote-actions)
  (clog :debug "printing-action %s" (format-action action))
  (insert (format-action action))
  (drop remote-actions action)
  ;;(backspace) 
(newline)))

(defun format-file (DB-rec)
  (format "%S %s %s %s %d %S"
	  (tilde (aref DB-rec plain))
	  (aref DB-rec cipher)
	  (aref DB-rec size)
	  (aref DB-rec gname)
	  (aref DB-rec modes); integer
	  (format-time-string "%F %H:%M:%S %Z" (aref DB-rec mtime))))

(defun cut-line() 
(prog1
(buffer-substring-no-properties (line-beginning-position) (line-end-position))
  (delete-region (line-beginning-position) (progn (safe-FL) (point)))))

(defun parse-action(str)
(clog :debug "parse-action(%s) ..." str)
(let ((action (make-vector (length action-fields) nil)))

(dolist (column (list
                 `(:time-stamp . ,i-time)
                 `(:int . ,i-ID)
                 `(:int . ,i-Nargs)))
  (needs ((col-value (begins-with str (car column)) (bad-column "action" (cdr column))))
     (aset action (cdr column) (car col-value))
     (setf str (cdr col-value))))

(dolist (column 
(list
  (cons (cons  :string  (aref action i-Nargs)) i-args)
       `(:strings . ,i-hostnames)))
  (needs ((col-value (begins-with str (car column)) (bad-column "action" (cdr column))))
     (aset action (cdr column) (car col-value)); was (mapcar #'untilde (car col-value))
     (setf str (cdr col-value))))

(let ((AID (format-time-string "%02m/%02d %H:%M:%S" (aref action i-time))))
(clog :info "... will later be referenced as %s" AID)
(cons AID action))))

(defun str-to-DBrec(str)
"parses one file line from the remote file DB"
(ifn (string-match "\"\\(.+\\)\"\s+\\([^\s]+\\)\s+\\([^\s]+\\)\s+\\([^\s]+\\)\s+\\([[:digit:]]+\\)\s+\"\\(.+\\)\"" str)
(clog :error "Ignoring invalid file line %s" str)

(let ((CF (make-vector (length file-fields) nil))
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
    (clog :warning "consider reloading configuration file %s" FN)
    ;;   (load-file FN))
)))

(defun cloud-connected-p()
  (and
   (remote-directory) (remote/files)
   (file-readable-p remote-directory)))
;;(file-readable-p (remote-files)

(defun write-all (DBname)
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
  (let (str)
(needs-set
 ((cloud-hosts
  (split-string (setf str (read-line)))
  (clog :error "invalid first line in the remote file DB %s" DBname)))

(unless (member localhost cloud-hosts) (cloud-host-add))

(while (< 0 (length (setf str (read-line))))
(clog :debug "action string=%s" str)
(when-let ((AA (parse-action str)) (AID (car AA)) (action (cdr AA)))
  (ifn (member localhost (aref action i-hostnames))
      (clog :info "this host is unaffected by action %s" AID)
    (if (perform action (aref action i-hostnames))
	(clog :info "sucessfully performed action %s" AID)
      (clog :error " action %s failed, will NOT retry it" AID))

(when (drop (aref action i-hostnames) localhost)
  (end-push action remote-actions)))))

(needs ((CDFs

(mapcar #'(lambda(s) (replace-regexp-in-string "....$" "" s))
      (directory-files remote-directory nil "...\...." t)) (clog :error "can not read %s" remote-directory)))
(while(< 10 (length (setf str (read-line))))
(when-let((CF (str-to-DBrec str)))

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
(upload CF)))))))
t)))))

(defun cloud-touch(&rest FNs)
"called when the file named FN is changed"
  (interactive)
(dolist(FN FNs)
(let((FR (cloud-locate-FN FN)))
(unless FR
  (auto-add-file FN)
  (setf FR (cloud-locate-FN FN)))
(when FR
    (aset FR mtime (current-time))
    (clog :debug "touch/upload: %s(%s)" FN (TS(aref FR mtime)))
    (upload FR)))))
(defun on-current-buffer-save()
  (when-let ((FN (buffer-file-name)))
    (cloud-touch FN)))
(add-hook 'after-save-hook 'on-current-buffer-save)

(defmacro NL() '(push "
" Makefile))
(defmacro inl (&rest format-pars) `(progn (push ,(cons 'format format-pars) Makefile) (NL)))
(define-vars (all Makefile uploaded stanze))

(defun cancel-pending-upload(FN) (drop stanze FN))
(defun pass-d () (to-dir (local-dir) "pass.d"))
(defun updated() (concat (pass-d) "updated"))

(defun enc-make-stanza(file-record)
  (when-let ((XYZ (aref file-record cipher)) (FN (tilde (aref file-record plain))))

(let ((file-ext (file-name-extension FN)))
(concat (cond

((member file-ext '("gz" "tgz"))
(let ((gunzipped (make-temp-file "emacs-cloud.")))
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
XYZ FN (updated)
(pass-d) XYZ))

(t 
(format "
$(cloud)%s.gpg: %s
\t@$(enc) $@ $<
" XYZ FN)))

"\t-@echo \"$$(date): uploaded $<\" >> $(localLog)
"))))

(defun dec-make-stanza(file-record)
  (when-let ((XYZ(aref file-record cipher)) (FN(tilde (aref file-record plain))))
    (let ((file-ext (file-name-extension FN)))
(concat
(cond

((string= "gpg" file-ext)
(format "
%s: $(cloud)%s.gpg
\tcp $< $@
" FN XYZ))

((member file-ext '("jpg" "jpeg" "png"))
(format "
%s: $(cloud)%s.png  %s
\tconvert $< -decipher %s%s $@
"
FN XYZ (updated)
(pass-d) XYZ))

((member file-ext '("gz" "tgz"))
(let ((gunzipped (make-temp-file "emacs-cloud.")))
  (format "
%s:$(cloud)%s.gpg
\t@$(dec) $@ $<

%s: %s
\tcat $< | gzip > $@
\trm $<
" 
gunzipped XYZ
FN gunzipped)))

((member file-ext '("bz2" "tbz"))
(let ((gunzipped (make-temp-file "emacs-cloud.")))
  (format "
%s:$(cloud)%s.gpg
\t@$(dec) $@ $<

%s: %s
\tcat $< | bzip2 > $@
\trm $<
" 
gunzipped XYZ
FN gunzipped)))

(t (format "
%s: $(cloud)%s.gpg
\t@$(dec) $@ $<
" FN XYZ)))

(format "\t-chgrp %s $@
\t-chmod %o $@
\t-touch --date=%S $@
\t-@echo \"$$(date): downloaded $@\" >> $(localLog)
"
(aref file-record gname) (aref file-record modes) (full-TS (aref file-record mtime)))))))

(defun download(FR)
(needs ((FN (aref FR plain) (clog :error "download: file lacks plain name"))
        (stanza (dec-make-stanza FR) (clog :error "download: could not create stanza for %s" FN)))
(ensure-dir-exists(file-name-directory FN))
(push FN stanze)
(push stanza Makefile) (NL)))

(defun make-cloud-older(FR)
(when-let ((FN (aref FR plain))
           (RN (concat (remote-directory) (aref FR cipher) (cip-ext FN)))
           (clouded (cloud-get-file-properties RN))
           (local-mtime (aref FR mtime)))
(when (time< local-mtime (aref clouded mtime))
(clog :debug "changing time stamp to %s" (TS(time-add local-mtime -60)))
  (set-file-times
;;(concat (remote-directory) (plain-name clouded) (cip-ext (plain-name FR)))
;; 01/06 replaced it with
(concat (remote-directory) (cipher-name clouded) (cip-ext (plain-name FR)))
(time-add local-mtime (- -60 (random 6000)))))))

(defun upload(FR)
(needs ((FN (tilde(aref FR plain)) (clog :error "upload: file lacks plain name"))
	(CN (aref FR cipher) (clog :error "upload: file %s lacks cipher name" FN))
	(stanza (enc-make-stanza FR) (clog :error "upload: could not create stanza for %s" FN)))
  (unless (or (member FN uploaded) (member FN file-blacklist))
    (push FN upload-queue)
    (make-cloud-older FR)
    (push FN uploaded)
    (push (format " %s" (concat (remote-directory) CN (cip-ext FN))) stanze)
    (push stanza Makefile) (NL))))

(defun reset-Makefile()
"reseting make file"
(when (or (and (file-exists-p(pass-d)) (file-directory-p(pass-d))) (ensure-dir-exists (pass-d)))
(setf stanze nil Makefile nil uploaded nil)
(inl "cloud=%s" remote-directory)
(inl "password=%S" password)
(inl "gpg=gpg --pinentry-mode loopback --batch --yes")
(inl "enc=$(gpg) --symmetric --passphrase $(password) -o")
(inl "dec=$(gpg) --decrypt   --passphrase $(password) -o")
(inl "localLog=%s" (tilde(local/log)))
(inl "MK=%s" (tilde(cloud-mk)))
(inl "date=`date '+%%m/%%d %%T'`
")
(inl (concat (format "%s: %s
\tawk '{print $$2 > %S$$1}' $<
\techo $(date) > $@
\t-chgrp -R tmp %s*
" (tilde(updated)) (tilde(image-passes)) (tilde(pass-d)) (tilde(pass-d)))))))

(defun save-Makefile()
"flushing make file"
(inl "all:%s
\techo \"background (en/de)cryption on %s finished $(date)\" >> %s
\t@sed 's/%s/******/g' %s > %s.bak
"
(together stanze)
localhost
(history)
password (cloud-mk) (cloud-mk))
(write-region (apply #'concat (reverse Makefile)) nil (untilde(cloud-mk))))

(defun cloud-sync()
(interactive)
(error-in "cloud-sync"

(defun do-make()
  (set-file-times (local/all) (current-time))
  (save-Makefile)
  (setf added-files nil upload-queue nil removed-files nil)
(let ((make (format "HOME=%s make -j%d -f %s all &> %s.log" HOME number-of-CPU-cores (untilde(cloud-mk)) (untilde(cloud-mk)))))

(ifn(= 0 (shell-command make)) (clog :error "make file containing
%s
FAILED with error(s): %s" (cat-file(untilde(cloud-mk))) (cat-file(concat(untilde(cloud-mk))".log")))
(delete-file(untilde(cloud-mk)))
(reset-Makefile))))

(ifn(cloud-connected-p) (clog :warning "refuse to sync because remote directory not mounted")
(directory-lock(lock-dir) (format "%s
%s" localhost (TS(current-time)))

(ifn (or (file-exists-p(remote-files)) (file-exists-p(local/all)))
(ifn (write-all(local/all)) (clog :error "could not save data to %s" (local/all))
(ifn(gpg-encrypt (local/all) (remote/files)) (error "could not encrypt %s to %s" (local/all) (remote/files))
(do-make))))

(when(file-newer-than-file-p (remote-files) (local/all))
(clog :debug "updating %s obsoleted by %s" (local/all) (remote-files))
(ifn(gpg-decrypt (local/all) (remote/files)) (error "could not DECRYPT file data FROM the cloud")
(read-all (local/all))))

(when (or added-files upload-queue removed-files)
  (ifn (write-all (local/all)) (error "could not save data to %s" (local/all))
    (ifn(gpg-encrypt (local/all) (remote/files)) (error "could not ENCRYPT file data TO the cloud"))))

(do-make))

(dolist (msg (reverse important-msgs)) (message msg))
(setf important-msgs nil)
(clog :info "done syncing")
(write-region (format "%s: %s -- %s
" localhost  (TS (current-time)) (format-time-string "%H:%M:%S" (current-time))) nil (history) t))))

(defun before-exit()
;; (write-conf)
(when (cloud-sync) (delete-directory /tmp/cloud/)))

(defun new-action (a-ID &rest args)
(mapcar #'(lambda(FN) (clog :debug "new-action(%d %s)" a-ID FN)) args)
  (let ((action (make-vector (length action-fields) nil)))
    (aset action i-ID a-ID)
    (aset action i-time (current-time))
    (aset action i-args args)
    (aset action i-hostnames (remove localhost cloud-hosts))
    (end-push action remote-actions)))

(defun perform(action &optional HNs)
"performing an action locally"
(write-region
(format "%s: %s
" (TS (current-time)) (format-action action))
nil (local/log) t)
  (let ((arguments (aref action i-args)))
    (case* (aref action i-ID) =
      (i-host-forget (dolist (arg arguments) (drop cloud-hosts arg)) t)
      (i-host-add (dolist (arg arguments) (push arg cloud-hosts)) t)
      (i-forget (cloud-forget-many arguments) t)
      (i-delete (cloud-rm arguments) t)
      (i-rename (cloud-rename-file (first arguments) (second arguments)) t)

(i-share (when (= 1 (length HNs)) (cloud-forget-many arguments)))
(otherwise (clog :error "unknown action %d" (aref action i-ID))))) t)

(defun format-action (action)
  (format "%S %d %d %s %s"
(full-TS (aref action i-time)); 1. Time stamp,
(aref action i-ID); 2. (integer) action ID,
(length (aref action i-args)); 3. (integer) number of arguments for this action (one column),
(apply #'concat (mapcar #'(lambda(arg) (format "%S " (tilde arg))) (aref action i-args))); 4. [arguments+] (several columns),
(apply #'concat (mapcar #'(lambda(HN) (format "%S " HN)) (aref action i-hostnames))))); 5. hostnames, where the action has to be performed (several columns).

(require 'nadvice)
;; (advice-remove #'dired-delete-file 'dired-delete-file@DDF)
(define-advice dired-delete-file (:after (FN &optional RECURSIVE TRASH) DDF)
   (cloud-forget FN)
   (unless(BRDp FN) (new-action i-delete FN)))

(defun cloud-rm (args)
(cloud-forget-many args)
(error-in "cloud-rm"
(dolist (arg args)
  (delete-directory arg t)
  (cloud-forget-recursive arg))))

(defun cloud-forget-many (args)
  (error-in "cloud-forget-many"
    (dolist (arg args)
      (unless(cloud-forget-recursive arg) (error "could not forget %s" arg)))))

(defun contained-in(DN)
  (let* ((dir-name (tilde DN)) res (dir-name (to-dir dir-name)))
    (dolist (DB-rec file-DB)
      (when(and
(< (length dir-name) (length (aref DB-rec plain)))
(string=(substring-no-properties (aref DB-rec plain) 0 (length dir-name)) dir-name))
        (push DB-rec res)))
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
  (unless (member localhost cloud-hosts)
    (push localhost cloud-hosts))
  (new-action i-host-add localhost)
  (add-to-actions localhost))

(defun cloud-host-forget()
  "remove host from the cloud sync-system"
    (when (yes-or-no-p (format "Forget the host %s?" localhost))
      (new-action i-host-forget localhost)
      (if(cloud-sync)
        (dired-delete-file (local/host/conf) "always")
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
(let ((FN (tilde FN)))
 (cloud-forget-file FN)
(unless (member FN file-blacklist)
 (push FN file-blacklist))))
(defun BRDp(FN)
  (when black-root-dirs(string-match (eval `(rx bol ,(cons 'or black-root-dirs))) FN)))
(defun black-p(FN &optional file-rec)
(let ((result
(or
 (member FN file-blacklist) 
 (string-match (rx (or "tmp" "/old/" "/.git/")) FN)
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
  (unless FR (setf FR (get-file-properties* FN)))
  (cons (member (aref FR gname) '("important" "keepOneYear" "keepTwoYears" "keepThreeYears")) FR))

(defun add-file(FN &optional file-rec)
(when FN
(let ((FN (untilde (file-chase-links FN))))
(unless (cloud-locate-FN FN)
(ifn (file-directory-p FN)
  (needs ((GFP (or file-rec (cloud-get-file-properties FN)) (clog :error "cannot cloud inexisting file %s" FN))
          (CN (new-file-name remote-directory)) (FN (tilde FN)))
    (push FN added-files)
    (aset GFP cipher CN)
    (push GFP file-DB) (clog :info "file %s is now clouded" FN)
    (upload GFP)
    (when (member (file-name-extension FN) '("jpeg" "png" "jpg"))

(write-region
  (format "%s %s
" CN (rand-str 18)) nil (image-passes) t)
(cloud-touch (image-passes))))

(let ((DN (to-dir FN)))
(dolist (FN (directory-files DN nil nil t))
(unless (member FN '("." ".."))
(let ((FN (concat DN FN)) FR)

(if (or
(let ((r (white-p FN))) (setf FR (cdr r)) (car r))
(not
(let ((r (black-p FN FR))) (setf FR (cdr r)) (car r))))
(add-file FN FR)
(clog :debug "not auto-clouding %s" FN)))))))))))

(defun auto-add-file(FN &optional file-rec)
"when the file is clouded automatically"
 (unless (car(black-p FN file-rec)) (add-file FN file-rec)))

(defun cloud-forget-file(FN)
  (needs ((DB-rec (cloud-locate-FN FN)
 (clog :warning "forget: doing nothing since %s is not clouded" FN))
          (CEXT (cip-ext FN))
	  (cloud-FN (concat(remote-directory) (aref DB-rec cipher) CEXT)))
(cancel-pending-upload FN)

(when (string= CEXT ".png")
  (forget-password (aref DB-rec cipher)))

(drop file-DB DB-rec)
(push FN removed-files)
(dired-delete-file cloud-FN "always")))

(defun cloud-forget-recursive(FN)
(new-action i-forget FN)
(dolist (sub-FN (mapcar #'plain-name (contained-in FN)))
  (cloud-forget-file sub-FN))
(cloud-forget-file FN))

(defun cloud-forget (&optional FN)
  (interactive)
(if FN (cloud-forget-recursive FN)
  (if (string= major-mode "dired-mode")
      (dired-map-over-marks(cloud-forget-recursive(dired-get-filename))nil)
(if-let ((FN (buffer-file-name))) (cloud-forget-recursive FN)
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
     (source (aset source plain new))
     (target (setf target (cloud-get-file-properties new))))))

(defun DRF (old-function old-FN new-FN ok-if-already-exists)
(let((isDir (file-directory-p old-FN)))
(error-in "DRF" (funcall old-function old-FN new-FN ok-if-already-exists)
(clog :debug "cloud-rename-file %s --> %s" old-FN new-FN)
(cloud-rename-file old-FN new-FN)
(unless(BRDp FN) (new-action i-rename old-FN new-FN))

(when isDir
  (let* ((old-dir (to-dir old-FN)) (LOD (length old-dir))
         (new-dir (to-dir new-FN)))
    (dolist (rec (contained-in old-FN))
      (let ((FN (aref rec plain)))
        (when (and (<= LOD (length FN))
	     (string= old-FN (substring FN 0 LOD)))
	  (let ((new-name (concat new-dir (substring FN LOD))))
            (cloud-rename-file FN new-name)
   (unless(BRDp FN)
	    (new-action i-rename FN new-name)))))))))))
(advice-add 'dired-rename-file :around #'DRF)

(defun cloud-start()
(save-some-buffers)
(ifn-let ((conf (read-conf)))
(progn
  (clog :warning "could not read local configuration file, trying to (re)create configuration")
  (when (cloud-init remote-directory)
    (clog :info "check newly created configuraion %s and then M-x cloud-start" (local/host/conf))))

(update-conf conf "remote-directory" "black-extensions" "black-root-dirs" "remote/files" "number-of-CPU-cores" "password")

(ifn (remote-directory) (clog :error "You have to set remote-directory for me before I can proceed")
(ifn password (clog :error "You have to set encryption password for me before I can proceed")

(add-hook 'kill-emacs-hook 'before-exit)

(unless (file-exists-p (image-passes))
  (write-region "" nil (image-passes))
  (add-file (image-passes)))

(reset-Makefile)
(when(file-exists-p(local/all)) (read-all (local/all)))
(cloud-sync)))))
