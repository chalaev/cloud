(defun cloud-init() "initializes cloud directory and generates password -- runs only once"
(clog :info "atempting to create new configuration for this host")
;;(when (yes-or-no-p "Is cloud mounted?")
;;(setf remote-directory (read-string "cloud directory=" remote-directory))
(ifn (member (safe-mkdir remote-directory) '(:exists t))
(clog :error "could not create/access directory %s" remote-directory)

(if (directory-files remote-directory nil "^.\+.gpg$" t)
    (clog :error "please clean the directory %s before asking me to initialize it" remote-directory)
(clog :info "creating (main) remote file DB in unused directory %s" remote-directory)
(ifn-set ((remote/files (new-file-name remote-directory)))
  (clog :error "could not create DB file in the directory %s" remote-directory)

(setf password (rand-str 9))

(ifn (member (safe-mkdir (local-dir)) '(:exists t))
  (clog :error "could not create/acess directory %s" (local-dir))
(write-conf)
(clog :info "use M-x cloud-add in the dired to cloud important files and directories" ))))))

(defun format-conf(CP)
(cond
  ((member CP numerical-parameters) (format "%s=%d" CP (symbol-value(intern CP))))
  ((member CP lists-of-strings) (format "%s=%s" CP
(apply #'concat (mapcar #'(lambda(item) (format "%s " item)) (sort (symbol-value(intern CP)) #'string<)))))
  (t (format "%s=%s" CP (symbol-value(intern CP))))))

(defun write-conf()
(clog :debug "starting write-conf")
(with-temp-file (local/config)
(mapcar #'(lambda(CP) (insert(format-conf CP)) (newline)) 
  '("remote-directory" "junk-extensions" "ignored-dirs" "remote/files" "number-of-CPU-cores" "password")))
(clog :debug "ended write-conf") t)

(defun read-conf* (file-name)
  "reads configuration file"
(with-temp-buffer
(safe-insert-file (local/config))
  (let (res str)
    (while (< 0 (length (setf str (read-line))))
     (if (string-match "^\\(\\ca+\\)=\\(\\ca+\\)$" str)
	 (push (cons (match-string 1 str) (match-string 2 str)) res)
       (clog :error "garbage string in configuration file: %s" str)))
    res)))

(defun read-conf()
  "reads configuration file"
(let ((conf (read-conf* (local/config))))
(ifn conf (clog :error "refuse to work until you specify 3-symbol contents name \"remote/files\" in %s" (local/config))
(dolist (CP (mapcar #'car conf))
(clog :debug "read-conf(%s)" CP)
  (setcdr (assoc CP conf)
    (cond
((member CP numerical-parameters) (string-to-number (cdr (assoc CP conf))))
((member CP lists-of-strings)  (split-string (cdr (assoc CP conf))))
(t (car (split-string (cdr (assoc CP conf))))))))
(clog :debug "done with read-conf")
conf)))
;; 2020-11-20 (car (split-string "/mnt/cloud/"))

(defun print-hosts()
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
	  (tilda (aref DB-rec plain))
	  (aref DB-rec cipher)
	  (aref DB-rec size)
	  (aref DB-rec gname)
	  (aref DB-rec modes); integer
	  (format-time-string "%F %H:%M:%S %Z" (aref DB-rec mtime))))

(defun safe-FL()
  (if (< (line-end-position) (point-max))
     (forward-line)
     (move-end-of-line 1)))
(defun read-line()
(let ((str (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
 (safe-FL)
 str))
(defun cut-line() 
(let ((str (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
  (delete-region (line-beginning-position) (progn (safe-FL) (point)))
   str))

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
     (aset action (cdr column) (car col-value)); was (mapcar #'untilda (car col-value))
     (setf str (cdr col-value))))

(let ((AID (format-time-string "%02m/%02d %H:%M:%S" (aref action i-time))))
(clog :info "... will later be referenced as %s" AID)
(cons AID action))))

(defun str-to-DBrec(str)
"parses one file line from the remote file DB"
(ifn (string-match "\"\\(.+\\)\"\s+\\([^\s]+\\)\s+\\([^\s]+\\)\s+\\([^\s]+\\)\s+\\([[:digit:]]+\\)\s+\"\\(.+\\)\"" str)
(clog :error "Ignoring invalid file line %s" str)

(let ((CF (make-vector (length file-fields) nil))
      (FN (match-string 1 str)))
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
  (temp-open DBname
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

(let* ((FN (plain-name CF))
       (CN (aref CF cipher))
       (remote-file-exists (member CN CDFs))
       (local-file-rec (or (cloud-locate-FN FN)
(and (not (member FN removed-files))
(when-let ((LF (get-file-properties* FN)))
        (aset LF cipher (aref CF cipher))
        (push LF file-DB)
        LF)))))
(cond
((not (or local-file-rec remote-file-exists))
 (clog :error "forgetting file %s which is marked as clouded but is neither on local disk nor in the cloud" FN)
 (drop file-DB CF))
((or
 (and (not local-file-rec) remote-file-exists)
 (and local-file-rec remote-file-exists (time< (aref local-file-rec mtime) (aref CF mtime))))
(when (and local-file-rec remote-file-exists)
  (clog :debug "read-all/download: %s(%s) is older than %s.gpg(%s)"
  (aref local-file-rec plain) (TS(aref local-file-rec mtime))
  (aref CF cipher) (TS(aref CF mtime))))

(if local-file-rec
   (aset local-file-rec mtime (aref CF mtime))
   (push CF file-DB))
(let*((DN(file-name-directory(aref CF plain))) (mkdir(safe-mkdir DN)))
(if(or(car mkdir)(eql :exists(cdr mkdir)))
(download CF)
(clog :error "could not mkdir %s" DN))))
((or
 (and local-file-rec remote-file-exists (time< (aref CF mtime) (aref local-file-rec mtime)))
 (and local-file-rec (not remote-file-exists)))
(when (and local-file-rec remote-file-exists)
  (clog :debug "read-all/upload: local %s(%s) is younger than %s.gpg(%s)"
  (aref local-file-rec plain) (TS(aref local-file-rec mtime))
  (aref CF cipher) (TS(aref CF mtime))))
(upload CF))))))
t)))))

(defun touch (FN)
"called when the file named FN is changed"
(when (and FN (stringp FN))
  (when-let ((file-data (cloud-locate-FN FN)))
    (aset file-data mtime (current-time))
    (clog :debug "touch/upload: %s(%s)" FN (TS(aref file-data mtime)))
    (upload file-data) t)))
(defun on-current-buffer-save()
  (when-let ((FN (buffer-file-name)))
    (and (auto-add-file FN) (touch FN))))
(add-hook 'after-save-hook 'on-current-buffer-save)

(macrolet ((NL () '(push "
" Makefile))
(inl (&rest format-pars) `(progn (push ,(cons 'format format-pars) Makefile) (NL))))
(let (all Makefile uploaded

(specially-encoded '(

("$(cloud)%s.gpg: %s
\tcp $< $@
" "gpg")

("$(cloud)%s.png: %s %s
\tconvert $< -encipher %s%s $@
" "jpg" "jpeg" "png")))

(specially-decoded '(
("%s: $(cloud)%s.gpg
\tcp $< $@
" "gpg")
("%s: $(cloud)%s.png  %s
\tconvert $< -decipher %s%s $@
" "jpg" "jpeg" "png"))))

(defun cancel-pending-upload(FN) (drop all FN))
(cl-labels ((pass-d () (concat (local-dir) "pass.d/"))
          (updated() (concat (pass-d) "updated")))

(cl-flet ((enc-make-stanza(file-record)
(when-let ((XYZ (aref file-record cipher)) (FN (tilda (aref file-record plain))))
(let ((file-ext (file-name-extension FN)))
(concat
(if-let ((fstr (car (find file-ext specially-encoded :key #'cdr :test #'(lambda(x y) (member x y))))))
(format fstr XYZ FN (updated) (pass-d) XYZ)

(if(string= "gz" file-ext)
(let ((gunzipped (make-temp-file "emacs-cloud.")))
  (format "%s: %s
\tzcat $< > $@

$(cloud)%s.gpg: %s
\t@$(enc) $@ $<
\trm $<
" gunzipped FN XYZ gunzipped))

(format "$(cloud)%s.gpg: %s
\t@$(enc) $@ $<
" XYZ FN)))

(format "\t-echo \"$(date): uploaded %s\" >> $(localLog)
" FN)))))

(dec-make-stanza(file-record)
(when-let ((XYZ (aref file-record cipher)) (FN (tilda (aref file-record plain))))
(let ((file-ext (file-name-extension FN)))
(concat
(if-let ((fstr (car (find file-ext specially-decoded :key #'cdr :test #'(lambda(x y) (member x y))))))
(format fstr FN XYZ (updated) (pass-d) XYZ)

(if(string= "gz" file-ext)
(let ((gunzipped (make-temp-file "emacs-cloud.")))
  (format "%s: %s
\tcat $< | gzip > $@

%s:$(cloud)%s.gpg
\t@$(enc) $@ $<
\trm $<
" FN gunzipped gunzipped XYZ))

(format "%s: $(cloud)%s.gpg
\t@$(dec) $@ $<
" FN XYZ ))
(format "\t-chgrp %s $@
\t-chmod %o $@
\t-touch --date=%S $@
\t-echo \"$(date): downloaded %s\" >> $(localLog)
" (aref file-record gname) (aref file-record modes) (full-TS (aref file-record mtime)) FN)))))))

(defun download (file-record)
(needs ((FN (aref file-record plain) (clog :error "download: file lacks plain name"))
        (stanza (dec-make-stanza file-record) (clog :error "download: could not create stanza for %s" FN)))
(safe-mkdir (file-name-directory FN))
(push (format " %s" FN) all)
(push stanza Makefile) (NL)))

(defun make-cloud-older(file-record)
(when-let ((clouded (get-file-properties (aref file-record cipher)))
           (local-mtime (aref file-record mtime)))
(when (time< local-mtime (aref clouded mtime))
(clog :debug "changing time stamp to %s" (FS (time-add local-mtime -60)))
  (set-file-times
(concat (remote-directory) (plain-name clouded) (cip-ext (plain-name file-record)))
(time-add local-mtime (- -60 (random 6000)))))))

(defun upload (file-record)
(needs ((FN (tilda (aref file-record plain)) (clog :error "upload: file lacks plain name"))
	(CN (aref file-record cipher) (clog :error "upload: file %s lacks cipher name" FN))
	(stanza (enc-make-stanza file-record) (clog :error "upload: could not create stanza for %s" FN)))
(clog :debug "started upload(%s)" FN)
(unless (or (member FN uploaded) (member FN *blacklist*))
(push FN upload-queue)
(clog :debug "will add upload(%s) stanza to Makefile" FN)
(make-cloud-older file-record)
(push FN uploaded)
(push (format " %s" (concat (remote-directory) CN
(cip-ext FN)))
all)
(push stanza Makefile) (NL))))

(defun reset-Makefile()
"reseting make file"
(when (or (and (file-exists-p (pass-d)) (file-directory-p (pass-d))) (safe-mkdir (pass-d)))
(setf all nil Makefile nil uploaded nil)
(inl "cloud=%s" remote-directory)
(inl "password=%S" password)
(inl "gpg=gpg --pinentry-mode loopback --batch --yes")
(inl "enc=$(gpg) --symmetric --passphrase $(password) -o")
(inl "dec=$(gpg) --decrypt   --passphrase $(password) -o")
(inl "localLog=%s" (local/log))
(inl "MK=%s" (cloud-mk))
(inl "date=`date '+%%m/%%d %%T'`
")
(inl (concat (format "%s: %s
\tawk '{print $$2 > %S$$1}' $<
\techo $(date) > $@
\t-chgrp -R tmp %s*
" (updated) (image-passes) (untilda (pass-d)) (pass-d))))))

(defun save-Makefile()
"flushing make file"
(inl "all:%s
\techo \"background (en/de)cryption on %s finished $(date)\" >> %s
\t@sed 's/%s/******/g' %s > %s.bak
"
(apply #'concat all)
localhost
(history)
password (cloud-mk) (cloud-mk))
(write-region (apply #'concat (reverse Makefile)) nil (cloud-mk)))))))

(defun cloud-sync()
(interactive)
(let ((ok t))

(ifn (cloud-connected-p) (clog :warning "refuse to sync because remote directory not mounted")

(let ((DL (directory-lock (lock-dir) (format "%s
%s" localhost (TS (current-time)))

(when (file-newer-than-file-p (remote-files) (local/all))
  (clog :info "detected NEW %s, will now update %s from it" (remote-files) (local/all))
  (unless (gpg-decrypt (local/all) (remote/files))
    (setf ok (clog :error "could not decrypt file data from the cloud; SHUT DOWN the service and INVESTIGATE!"))))

(unless (read-all (local/all))
 (setf ok (clog :error "could not parse file data just downloaded from the cloud; SHUT DOWN the service and INVESTIGATE!")))

(when (or added-files upload-queue removed-files)
  (ifn (write-all (local/all)) (setf ok (clog :error "could not save data to %s" (local/all)))
    (gpg-encrypt (local/all) (remote/files))
    (setf added-files nil upload-queue nil removed-files nil)))

(set-file-times (local/all) (current-time))

(save-Makefile)
(let ((make (format "make -j%d -f %s all &> %s.log" number-of-CPU-cores (cloud-mk) (cloud-mk))))
  (clog :debug "starting %s" make)
  (shell-command make)
  (clog :debug "finished %s" make))
(rm (cloud-mk))
(reset-Makefile))))

(unless (car DL) (setf ok (clog :error "Could not (un)lock remote directory! Please investigate"))))

(dolist (msg (reverse important-msgs)) (message msg))
(setf important-msgs nil)
(clog :info "done syncing")
(write-region (format "%s: %s -- %s
" localhost  (TS (current-time)) (format-time-string "%H:%M:%S" (current-time))) nil (history)))
ok))

(defun before-exit()
;; (write-conf)
(when (cloud-sync)
  (safe-delete-dir /tmp/cloud/)))

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
(apply #'concat (mapcar #'(lambda(arg) (format "%S " (tilda arg))) (aref action i-args))); 4. [arguments+] (several columns),
(apply #'concat (mapcar #'(lambda(HN) (format "%S " HN)) (aref action i-hostnames))))); 5. hostnames, where the action has to be performed (several columns).

(defun dired-delete-file (FN &optional dirP TRASH)
  (let ((FN (tilda FN))); ~/programming/emacs/functions.el
(when (car    
       (condition-case err (cons t (funcall DDF FN dirP TRASH))
	 (file-error (clog :error "in DDF: %s" (error-message-string err)))))
  (cons t (and (cloud-forget-recursive FN)
	       (new-action i-delete FN))))))

(defun cloud-rm (args)
(let ((ok (cloud-forget-many args)))
  (dolist (arg args)
    (setf ok (and (safe-delete-dir arg t) (cloud-forget-recursive arg) ok)))
ok))

(defun cloud-forget-many (args)
  (interactive) 
(let ((ok t))
  (dolist (arg args)
    (setf ok (and (cloud-forget-recursive arg) ok)))
ok))

(defun contained-in(DN)
  (let* ((dir-name (tilda DN)) res (dir-name (file-name-as-directory dir-name)))
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

(defun cloud-host-forget ()
  "remove host from the cloud sync-system"
    (when (yes-or-no-p (format "Forget the host %s?" localhost))
      (new-action i-host-forget localhost)
      (if (cloud-sync)
	  (safe-dired-delete (local/config))
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
(let ((FN (tilda FN)))
 (cloud-forget-file FN)
(unless (member FN *blacklist*)
 (push FN *blacklist*))))
(defun black-p(FN &optional file-rec)
(let ((result
(or
(member FN *blacklist*) (string-match "tmp" FN)
(string-match (concat ~ ".") (untilda FN))
(member (file-name-extension FN) junk-extensions)
(backup-file-name-p FN)
(when ignored-dirs (string-match(substring(apply #'concat
  (mapcar #'(lambda(d)(format "\\(^%s\\)\\|" d)) ignored-dirs)) 0 -2) FN))
(progn
  (unless file-rec (setf file-rec (get-file-properties FN)))
(when file-rec
  (or
    (member (aref file-rec gname) '("tmp"))
    (< 1000000 (aref file-rec size))))))))
(cons result file-rec)))

(defun white-p(FN &optional file-rec)
(unless file-rec (setf file-rec (get-file-properties FN)))
(cons (member (aref file-rec gname) '("important" "keepOneYear" "keepTwoYears" "keepThreeYears")) file-rec))

(defun add-file(FN &optional file-rec)
(let ((FN (tilda (file-chase-links FN))))
(unless (cloud-locate-FN FN)
(ifn (file-directory-p FN)
  (needs ((GFP (or file-rec (get-file-properties* FN)) (clog :error "Aborting attempt to cloud inexisting file %s" FN))
          (CN (new-file-name remote-directory)))
(push FN added-files)
    (aset GFP cipher CN)
    (push GFP file-DB)
    (clog :debug "add-file/upload: %s(%s)" FN (TS(aref GFP mtime)))
    (upload GFP)
    (when (member (file-name-extension FN) '("jpeg" "png" "jpg"))

(write-region
  (format "%s %s
" CN (rand-str 18)) nil (image-passes) t)
(touch (image-passes))))

(let ((DN (file-name-as-directory FN)))
(dolist (FN (directory-files DN nil nil t))
(unless (member FN '("." ".."))
(let ((FN (concat DN FN)) FR)

(if (or
(let ((r (white-p FN))) (setf FR (cdr r)) (car r))
(not
(let ((r (black-p FN FR))) (setf FR (cdr r)) (car r))))
(add-file FN FR)
(clog :debug "not auto-clouding %s" FN))))))))))

(defun auto-add-file(FN &optional file-rec)
"when the file is clouded automatically"
 (unless (car(black-p FN file-rec)) (add-file FN file-rec)) t)

(defun cloud-forget-file (local-FN); called *after* the file has already been sucessfully deleted
  (needs ((DB-rec (or (cloud-locate-FN local-FN) (old-cloud-locate-FN local-FN))
 (clog :warning "forget: doing nothing since %s is not clouded" local-FN))
          (CEXT (cip-ext local-FN))
	  (cloud-FN (concat (remote-directory) (aref DB-rec cipher) CEXT) (clog :error "in DB entry for %s" local-FN)))
(cancel-pending-upload local-FN)

(when (string= CEXT ".png")
(clog :debug "forgetting password for %s" local-FN)
  (forget-password (aref DB-rec cipher)))

(drop file-DB DB-rec)
(push local-FN removed-files)
(if (car (safe-dired-delete cloud-FN))
  (clog :info "erased %s" cloud-FN)
  (clog :warning "could not erase %s" cloud-FN))
 t))

(defun cloud-forget-recursive(FN)
(clog :debug "cloud-forget-recursive(%s)" FN)
(new-action i-forget FN)
(dolist (sub-FN (mapcar #'plain-name (contained-in FN)))
  (cloud-forget-file sub-FN))
(cloud-forget-file FN))

(defun cloud-forget (&optional FN)
  (interactive)
(if FN (cloud-forget-recursive FN)
  (if (string= major-mode "dired-mode")
      (dired-map-over-marks (cloud-forget-recursive (dired-get-filename)) nil)
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
     (target (setf target (get-file-properties* new))))))

(defun dired-rename-file (old-FN new-FN ok-if-already-exists)
  (let (failure)
    (clog :debug "DRF")
    (condition-case err
	(funcall DRF old-FN new-FN ok-if-already-exists)
      (file-error
       (clog :error "DRF error!")
       (message "%s" (error-message-string err))
       (setf failure t)))
    (unless failure
      (clog :debug "cloud-rename-file %s --> %s" old-FN new-FN)
      (cloud-rename-file old-FN new-FN)
      (new-action i-rename old-FN new-FN)

(when (file-directory-p old-FN)
  (let* ((old-dir (file-name-as-directory old-FN)) (LOD (length old-dir))
         (new-dir (file-name-as-directory new-FN)))
    (dolist (rec (contained-in old-FN))
      (let ((FN (aref rec plain)))
        (when (and (<= LOD (length FN))
	     (string= old-FN (substring FN 0 LOD)))
	  (let ((new-name (concat new-dir (substring FN LOD))))
            (cloud-rename-file FN new-name)
	    (new-action i-rename FN new-name))))))))))

(defun rename-directory (old-dir new-dir)
"recursively update plain-names of clouded files due to renaming of a directory"
(when (file-directory-p old-dir)
  (let* ((old-dir (file-name-as-directory old-dir)) (LOD (length old-dir))
         (new-dir (file-name-as-directory new-dir)))
    (dolist (rec (contained-in old-dir))
      (let ((FN (aref rec plain)))
        (when (and (<= LOD (length FN))
		   (string= old-dir (substring FN 0 LOD)))
	  (aset rec plain (concat new-dir (substring FN LOD)))))))))

(defun update-conf(conf &rest conf-params)
(clog :debug "started update-conf")
  (dolist (CP conf-params)
    (when-let ((CPV (cdr (assoc CP conf)))) (set (intern CP) CPV)))
(clog :debug "ended update-conf"))

(defun cloud-start()
(save-some-buffers)
(clog :debug "cloud-start: local/config = %s" (local/config))
(ifn-let ((conf (read-conf)))
(progn
  (clog :warning "could not read local configuration file, trying to (re)create configuration")
  (when (cloud-init)
  (clog :info "check newly created configuraion %s and then M-x cloud-start" (local/config))))

(update-conf conf "remote-directory" "junk-extensions" "ignored-dirs" "remote/files" "number-of-CPU-cores" "password")

(ifn remote-directory (clog :error "You have to set remote-directory for me before I can proceed")
(ifn password (clog :error "You have to set encryption password for me before I can proceed")

(add-hook 'kill-emacs-hook 'before-exit)

(unless (file-exists-p (image-passes))
  (write-region "" nil (image-passes))
  (add-file (image-passes)))

(reset-Makefile)
(cloud-sync)))))

(defun read-fileDB()
(clog :debug "starting read-fileDB")
(or
(and
;; (cloud-connected-p)
(= 0 (apply #'call-process
(append (list "gpg" nil nil nil)
(split-string (format
"--batch --yes --pinentry-mode loopback --passphrase %s -o %s --decrypt %s"

password (untilda (local/all)) (remote-files))))))
(read-all (local/all)))
(clog :error "cloud-start header failed") nil))
