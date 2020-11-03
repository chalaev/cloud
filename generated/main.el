;;; -*- mode: Emacs-Lisp;  lexical-binding: t; -*-
;; generated from cloud.org
(defvar password nil); to be read from config or generated
(defvar N-CPU-cores 1)
(defvar cloud-file-hooks nil "for special files treatment")

(defvar upload-queue nil "names of edited files")
(defvar added-files nil "newly clouded files")

(defvar remote/files nil "encrypted DB (without .gpg suffix) stored on the server")
(defvar remote-dir  "/mnt/cloud/")
(defun remote/files() remote/files)
(defun remote-dir() remote-dir)
(defun remote-files() (concat (remote-dir) remote/files ".gpg"))

(defvar emacs-d "~/.emacs.d/")
(defvar cloud-was-connected t "normally t, nill when there was no connection")

(defun local-dir() (concat emacs-d "cloud/"))
(defun cloud-mk() (concat (local-dir) "cloud.mk"))
(defun lock-dir() (concat (remote-dir) "now-syncing/"))
(defun lock-file() (concat (lock-dir) (system-name)))
(defun image-passes() (concat (local-dir) "individual.passes"))
(defun local/() (concat (local-dir) (system-name) "/"))
(defun local/log() (concat (local/) "log"))

(defun local/config() (concat (local-dir) (system-name) "/config"))

(defun cloud-init() "initializes cloud directory and generates password -- runs only once"
(clog :info "atempting to create new configuration for this host")
;;(when (yes-or-no-p "Is cloud mounted?")
;;(setf remote-dir (read-string "cloud directory=" remote-dir))
(ifn (member (safe-mkdir remote-dir) '(:exists t))
(clog :error "could not create/access directory %s" remote-dir)

(if (directory-files remote-dir nil "^.\+.gpg$" t)
    (clog :error "please clean the directory %s before asking me to initialize it" remote-dir)
(clog :info "creating (main) remote file DB in unused directory %s" remote-dir)
(ifn-set ((remote/files (new-file-name remote-dir)))
  (clog :error "could not create DB file in the directory %s" remote-dir)

(setf password (rand-str 9))

(ifn (member (safe-mkdir (local-dir)) '(:exists t))
  (clog :error "could not create/acess directory %s" (local-dir))
(write-conf)
(clog :info "use M-x cloud-add in the dired to cloud important files and directories" ))))))

(defun write-conf()
(clog :debug "starting write-conf")
(with-temp-file (local/config)
  (insert (format "remote/files=%s" (remote/files))) (newline)
  (insert (format "password=%s" password)) (newline)
  (insert (format "number-of-CPU-cores=%s" N-CPU-cores)) (newline)
  (insert (format "remote-directory=%s" remote-dir)) (newline)))

(defun read-conf()
  "reads configuration file"
(let (conf)
(needs-set
  ((conf (read-conf* (local/config)))
   (remote/files (cdr (assoc "remote/files" conf)) (clog :error "specify 3-symbol contents name \"remote/files\" in %s" (local/config)))
   (N-CPU-cores (string-to-number
 (or
 (cdr (assoc "number-of-CPU-cores" conf))
 (clog :warning "specify number-of-CPU-cores in %s" (local/config)
 "1"))))
   (password (cdr (assoc "password" conf)) (clog :error "specify password in %s" (local/config)))
   (remote-dir (cdr (assoc "remote-directory" conf)) (clog :error "specify remote-directory in %s" (local/config))))
conf)))

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

(defvar cloud-hosts nil "host names participating in file synchronization")
(defvar remote-actions nil "actions to be saved in the cloud")
(defvar file-DB nil "list of vectors, each corresponding to a clouded file")

(defvar file-fields; indices numerating array fields
(list 'plain; original (local) file name
'cipher; encrypted file name (base name)
'mtime; modification time
'modes; permissions
'uname; user name (obsolete and unused)
'gname)); group name
(let ((i 0)) (dolist (field-name file-fields) (setf i (1+ (set field-name i)))))

(defun local/all() (concat (local/) "all"))

(defun print-hosts()
(dolist (hostname cloud-hosts) (insert (format "%s " hostname)))
(backspace)
(newline))
;;(clog :debug "print-hosts finished"))

(defun print-actions()
(clog :debug "print-action started")
(dolist (action (reverse remote-actions))
  (insert (format-action action))
  (drop remote-actions action)
  ;;(backspace) 
(newline)))

(defun format-file (DB-rec)
  (format "%S %s %s %s %d %S"
	  (tilda (aref DB-rec plain))
	  (aref DB-rec cipher)
	  (aref DB-rec uname)
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
  (aset CF uname (match-string 3 str))

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

(defvar removed-files  nil "files that were just removed (or renamed) on local host before (cloud-sync)")

(defvar important-msgs nil "these messages will be typically printed at the end of the process")
(defvar gpg-process nil "assyncronous make-process for (en/de)cryption")

(defun cloud-connected-p()
  (and
   (remote-dir) (remote/files)
   (file-readable-p remote-dir)))
;;(file-readable-p (remote-files)

(defun write-all (DBname)
  (with-temp-file DBname
(print-hosts)

(print-actions)

(newline)

(dolist (file-record file-DB)
  (insert (format-file file-record)) (newline))
(setf removed-files nil) t))

(defun read-all (DBname)
  "reads content (text) file into the database file-DB"
  (temp-open DBname
  (let (str)
(needs-set
 ((cloud-hosts
  (split-string (setf str (read-line)))
  (clog :error "invalid first line in the remote file DB %s" DBname)))

(unless (member (system-name) cloud-hosts) (cloud-host-add))

(while (< 0 (length (setf str (read-line))))
(when-let ((AA (parse-action str)) (AID (car AA)) (action (cdr AA)))
  (ifn (member (system-name) (aref action i-hostnames))
      (clog :info "this host is unaffected by action %s" AID)
    (if (perform action)
	(clog :info "sucessfully performed action %s" AID)
      (clog :error " action %s failed, will NOT retry it" AID))

(when (drop (aref action i-hostnames) (system-name))
  (push action remote-actions)))))

;;(forward-line)
(needs ((CDFs

(mapcar #'(lambda(s) (replace-regexp-in-string "....$" "" s))
      (directory-files remote-dir nil "...\...." t)) (clog :error "can not read %s" remote-dir)))
(while (< 10 (length (setf str (read-line))))
(when-let ((CF (str-to-DBrec str)))

(let* ((FN (plain-name CF))
      (remote-file-exists (member FN CDFs))
      (local-file-rec (or (cloud-locate-FN FN)
(when-let ((LF (get-file-properties* FN)))
        (aset LF cipher (aref CF cipher))
        (push LF file-DB)
        LF))))
(cond
((not (or local-file-rec remote-file-exists))
 (clog :error "forgetting file %s which is marked as clouded but is neither on local disk nor in the cloud" FN)
 (drop file-DB CF))
((or
 (and (not local-file-rec) remote-file-exists)
 (and local-file-rec remote-file-exists (time< (aref local-file-rec mtime) (aref CF mtime))))

(download CF))
((or
 (and local-file-rec remote-file-exists (time< (aref CF mtime) (aref local-file-rec mtime)))
 (and local-file-rec (not remote-file-exists)))
(upload CF))))))
t)))))

(defun touch (FN)
"called when the file named FN is changed"
(clog :debug "touch(%s)" FN)
(when (and FN (stringp FN))
  (when-let ((file-data (cloud-locate-FN (file-chase-links FN))))
    (aset file-data mtime (current-time))
    (upload file-data) t)))
(defun on-current-buffer-save()
  (touch (buffer-file-name)))
(add-hook 'after-save-hook 'on-current-buffer-save)

(macrolet ((NL () '(push "
" Makefile))
(inl (&rest format-pars) `(progn (push ,(cons 'format format-pars) Makefile) (NL))))
(cl-flet ((pass-d()  (concat (local-dir) "pass.d/")))
(cl-flet ((updated() (concat (pass-d) "updated")))
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

(cl-flet ((enc-make-stanza(file-record)
(when-let ((XYZ (aref file-record cipher)) (FN (tilda (aref file-record plain))))
(let ((file-ext (file-name-extension FN)))
(concat
(if-let ((fstr (car (find file-ext specially-encoded :key #'cdr :test #'(lambda(x y) (member x y))))))
(format fstr XYZ FN (updated) (pass-d) XYZ)

(format "$(cloud)%s.gpg: %s
\t@$(enc) $@ $<
" XYZ FN))

(format "\t-echo \"$(date): uploaded %s\" >> $(localLog)
" FN)))))

(dec-make-stanza(file-record)
(when-let ((XYZ (aref file-record cipher)) (FN (tilda (aref file-record plain))))
(let ((file-ext (file-name-extension FN)))
(concat
(if-let ((fstr (car (find file-ext specially-decoded :key #'cdr :test #'(lambda(x y) (member x y))))))
(format fstr FN XYZ (updated) (pass-d) XYZ)
(format "%s: $(cloud)%s.gpg
\t@$(dec) $@ $<
" FN XYZ ))
(format "\t-chgrp %s $@
\t-chmod %o $@
\t-touch --date=%S $@
\t-echo \"$(date): downloaded %s\" >> $(localLog)
" (aref file-record gname) (aref file-record modes) (full-TS (aref file-record mtime)) FN))))))

(defun download (file-record)
(needs ((FN (aref file-record plain) (clog :error "download: file lacks plain name"))
        (stanza (dec-make-stanza file-record) (clog :error "download: could not create stanza for %s" FN)))
(push (format " %s" FN) all)
(push stanza Makefile) (NL)))

(defun make-cloud-older(file-record)
;;(clog :debug "make-cloud-older(%s)" (plain-name file-record))
(when-let ((clouded (get-file-properties (aref file-record cipher)))
           (local-mtime (aref file-record mtime)))
(when (time< local-mtime (aref clouded mtime))
(clog :debug "changing time stamp to %s" (FS (time-add local-mtime -60)))
  (set-file-times
(concat (remote-dir) (plain-name clouded) (cip-ext (plain-name file-record)))
(time-add local-mtime (- -60 (random 6000)))))))

(defun upload (file-record)
(needs ((FN (tilda (aref file-record plain)) (clog :error "upload: file lacks plain name"))
	(CN (aref file-record cipher) (clog :error "upload: file %s lacks cipher name" FN))
	(stanza (enc-make-stanza file-record) (clog :error "upload: could not create stanza for %s" FN)))
;;(clog :debug "started upload(%s)" FN)
(unless (member FN uploaded)
(push FN upload-queue)
;;(clog :debug "will indeed upload(%s)" FN)
(make-cloud-older file-record)
(push FN uploaded)
(push (format " %s" (concat (remote-dir) CN
(cip-ext FN)))
all)
(push stanza Makefile) (NL))))

(defun reset-Makefile()
"reseting make file"
(when (or (and (file-exists-p (pass-d)) (file-directory-p (pass-d))) (safe-mkdir (pass-d)))
(setf all nil Makefile nil uploaded nil)
(inl "cloud=%s" remote-dir)
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
(system-name)
(concat (remote-dir) "history")
password (cloud-mk) (cloud-mk))
(write-region (apply #'concat (reverse Makefile)) nil (cloud-mk))))))))

(defun cloud-sync()
(interactive)
(let ((ok t))

(unless file-DB
 (clog :info "loading data from disk at start")
 (read-all (local/all)))

(ifn (cloud-connected-p) (clog :warning "remote directory not mounted, so we will not encrypt %s-->%s" (local/all) (remote-files))
  (directory-lock (lock-dir)
    (when (file-newer-than-file-p (remote-files) (local/all))
      (clog :info "detected NEW %s, will now update %s from it" (remote-files) (local/all))
      (ifn (gpg-decrypt (local/all) (remote/files))
	(setf ok (clog :error "could not decrypt file data from the cloud; SHUT DOWN the service and INVESTIGATE!"))
	(unless (read-all (local/all))
	  (setf ok (clog :error "could not parse file data from the cloud; SHUT DOWN the service and INVESTIGATE!")))))

(when (or added-files upload-queue removed-files)
  (ifn (write-all (local/all)) (setf ok (clog :error "could not save data to %s" (local/all)))
    (gpg-encrypt (local/all) (remote/files))
    (setf added-files nil upload-queue nil)))

(set-file-times (local/all) (current-time))

(save-Makefile)
(let ((make (format "make -j%d -f %s all &> %s.log" N-CPU-cores (cloud-mk) (cloud-mk))))
  (clog :debug "starting %s" make)
  (shell-command make)
  (clog :debug "finished %s" make))
(reset-Makefile)))

(dolist (msg (reverse important-msgs)) (message msg))
(setf important-msgs nil)
(clog :info "done syncing")
(write-region (format "%s: %s -- %s
" (system-name)  (TS (current-time)) (format-time-string "%H:%M:%S" (current-time))) nil (concat (remote-dir) "history"))
ok))

(defun before-exit()
;; (write-conf)
  (cloud-sync))

(defvar action-fields '(i-time i-ID i-args i-hostnames i-Nargs))
(let ((i 0)) (dolist (AF action-fields) (setf i (1+ (set AF i)))))

(defvar action-IDs '(i-forget i-delete i-rename i-host-add i-host-forget))
(let ((i 0)) (dolist (AI action-IDs) (setf i (1+ (set AI i)))))

(defun new-action (a-ID &rest args)
  (let ((action (make-vector (length action-fields) nil)))
    (aset action i-ID a-ID)
    (aset action i-time (current-time))
    (aset action i-args args)
    (aset action i-hostnames (remove (system-name) cloud-hosts))
    (push action remote-actions)))

(defun perform(action)
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
      (otherwise (clog :error "unknown action %d" (aref action i-ID)))))
   (drop remote-actions action) t)

(defun format-action (action)
  (format "%S %d %d %s %s"
(full-TS (aref action i-time)); 1. Time stamp,
(aref action i-ID); 2. (integer) action ID,
(length (aref action i-args)); 3. (integer) number of arguments for this action (one column),
(apply #'concat (mapcar #'(lambda(arg) (format "%S " (tilda arg))) (aref action i-args))); 4. [arguments+] (several columns),
(apply #'concat (mapcar #'(lambda(HN) (format "%S " HN)) (aref action i-hostnames))))); 5. hostnames, where the action has to be performed (several columns).

(unless (boundp 'DRF) (defvar DRF (indirect-function (symbol-function 'dired-rename-file)) "original dired-rename-file function"))
(unless (boundp 'DDF) (defvar DDF (indirect-function (symbol-function 'dired-delete-file)) "original dired-delete-file function"))

(defun dired-delete-file (FN &optional dirP TRASH)
  (let (failure (FN (tilda FN)))

(condition-case err (funcall DDF FN dirP TRASH)
  (file-error
    (clog :error "in DDF: %s" (error-message-string err))
    (setf failure t)))
(unless failure

(cloud-forget-recursive FN) (new-action i-delete FN)
(when dirP
  (dolist (sub-FN (mapcar #'plain-name (contained-in FN)))
    (when (cloud-forget-file sub-FN) (new-action i-delete sub-FN)))))))

(defun cloud-rm (args)
  (interactive) 
(let ((ok (cloud-forget-many args)))
  (dolist (arg args)
    (setf ok (and (safe-dired-delete arg) (cloud-forget-recursive arg) ok)))
ok))

(defun cloud-forget-many (args)
  (interactive) 
(let ((ok t))
  (dolist (arg args)
    (setf ok (and (cloud-forget-recursive arg) ok)))
ok))

(defun contained-in(dir-name); dir-name must end with a slash /
    (let (res)
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
(let ((hostname (system-name)))
  (unless (member hostname cloud-hosts)
    (push hostname cloud-hosts))
  (new-action i-host-add hostname)
  (add-to-actions hostname)))

(defun cloud-host-forget ()
  "remove host from the cloud sync-system"
  (let ((hostname (system-name)))
    (when (yes-or-no-p (format "Forget the host %s?" hostname))
      (new-action i-host-forget hostname)
      (if (cloud-sync)
	  (safe-dired-delete (local/config))
	(clog :error "sync failed, so I will not erase local configuration")))))

(defun cloud-add (&optional FN)
  (interactive)
  (if (string= major-mode "dired-mode")
      (dired-map-over-marks (add-files (dired-get-filename)) nil)
    (unless
	(add-files (read-string "file to be clouded=" (if FN FN "")))
      (clog :error "could not cloud this file"))))

(defun add-file(FN)
(let ((FN (tilda FN)))
(unless (cloud-locate-FN FN)
  (needs ((GFP (get-file-properties* (file-chase-links FN)) (clog :error "Aborting attempt to cloud inexisting file %s" FN))
          (CN (new-file-name remote-dir)))
(push FN added-files)
    (aset GFP cipher CN)
    (push GFP file-DB)
    (upload GFP)
    (when (member (file-name-extension FN) '("jpeg" "png" "jpg"))

(write-region
  (format "%s %s
" CN (rand-str 18)) nil (image-passes) t)
(touch (image-passes)))))))

(defun add-files(&rest names)
(when
  (dolist (FN names)
    (unless (cloud-locate-FN FN)
      (needs ((GFP (get-file-properties* (file-chase-links FN)) (clog :error "Aborting attempt to cloud inexisting file %s" FN))
              (CN (new-file-name remote-dir)))
	(aset GFP cipher CN)
	(push GFP file-DB)
	(when (member (file-name-extension FN) '("jpeg" "png" "jpg"))

(write-region
  (format "%s %s
" CN (rand-str 18)) nil (image-passes) t)
(touch (image-passes)))
(upload GFP))))
;;(save-Makefile)
ok))

(defun cloud-forget-file (local-FN); called *after* the file has already been sucessfully deleted
   (push local-FN removed-files)
  (needs ((DB-rec (cloud-locate-FN local-FN) (clog :info "forget: doing nothing since %s is not clouded" local-FN))
          (CEXT (cip-ext local-FN))
	  (cloud-FN (concat (remote-dir) (aref DB-rec cipher) CEXT) (clog :error "in DB entry for %s" local-FN)))

(when (string= CEXT ".png")
(clog :debug "forgetting password for %s" local-FN)
  (forget-password (aref DB-rec cipher)))

(drop file-DB DB-rec)
(push local-FN removed-files)
(safe-dired-delete cloud-FN)
 t))

(defun cloud-forget-recursive(FN)
(dolist (sub-FN (mapcar #'plain-name (contained-in FN)))
(cloud-forget-file sub-FN)))

(defun cloud-forget (&optional FN)
  (interactive)
  (if (string= major-mode "dired-mode")
      (dired-map-over-marks (cloud-forget-recursive (dired-get-filename)) nil)
    (unless
	(cloud-forget-recursive (read-string "file to be forgotten=" (if FN FN "")))
      (clog :error "could not forget this file"))))

(defun cloud-rename-file (old new)
  (let ((source (cloud-locate-FN old))
        (target (cloud-locate-FN new)))
(cloud-forget-recursive old)
    (cond
     ((and source target); overwriting one cloud file with another one
      (dolist (property (list mtime modes uname gname)) do
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
       (clog :debug "DRF error!")
       (message "%s" (error-message-string err))
       (setf failure t)))
    (unless failure
      (clog :debug "launching my cloud rename %s --> %s" old-FN new-FN)
      (cloud-rename-file old-FN new-FN)
      (new-action i-rename old-FN new-FN))))

(defun cloud-start()
  (interactive) (save-some-buffers)
(clog :debug "cloud-start: local/config = %s" (local/config))
(ifn-let ((conf (read-conf)))
(progn
  (clog :warning "could not read local configuration file, trying to (re)create configuration")
  (when (cloud-init)
  (clog :info "check newly created configuraion %s and then M-x cloud-start" (local/config))))

(ifn (and
   (if-let ((CD (cdr (assoc "remote-directory" conf))))
	  (setf remote-dir CD)
	(setf remote-dir (read-string "cloud directory=" remote-dir))
	(write-conf) t)
   (setf remote/files (cdr (assoc "remote/files" conf)))
   (setf N-CPU-cores (string-to-number (or (cdr (assoc "number-of-CPU-cores" conf)) "1")))
   (setf password  (cdr (assoc "password" conf))))
(clog :error "something is missing or wrong in the configuration file" remote-dir)

(setf remote-dir 
  (or (cdr (assoc "remote-directory" conf))
      (read-string "cloud directory=" remote-dir)))

(add-hook 'kill-emacs-hook 'before-exit)

(unless (file-exists-p (image-passes))
  (write-region "" nil (image-passes))
  (add-files (image-passes)))

(reset-Makefile)
(cloud-sync))))

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
