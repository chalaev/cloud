
;; generated from cloud.org
(defvar cloud-delete-contents t "if decrypted contents file must be erased")
(defvar *clouded-hosts* nil "host names participating in file syncronization")
(defvar *pending-actions* nil "actions to be saved in the cloud")
(defvar *removed-files*  nil "files that were just removed (or renamed) on local host before (cloud-sync)")
(defvar *important-msgs* nil "these messages will be typically printed at the end of the process")
(defvar ~ (getenv "HOME"))
(defvar *gpg-process* nil "assyncronous make-process for (en/de)cryption")

(defvar cloud-file-hooks nil "for special files treatment")
(defvar *local-dir* (concat *emacs-d* "cloud/"))
(defvar *cloud-mk* (concat *local-dir* "cloud.mk"))
(defvar *Ncores* 1)
(defvar *cloud-dir*  "/mnt/cloud/")
(defvar cloud-lockdir (concat *cloud-dir* "now-syncing/"))
(defvar cloud-lockfile (concat cloud-lockdir (system-name)))

(defvar *local-config* (concat *local-dir* "config"))

(defvar *contents-name* nil)

(defvar *file-DB* nil "")
(defvar *password* nil); to be read from config or generated
;;(defvar *catalogue* nil); randomly generated name of the catalogue
;;(defvar *encrypted-files* nil "list of the files after they are encrypted")
(defvar DB-fields; indices numerating array fields
(list 'plain; original (local) file name
'cipher; encrypted file name (base name)
'mtime; modification time
'modes; permissions
'uname; user name (obsolete and unused)
'gname; group name
))
(let ((i 0)) (dolist (field-name DB-fields) (setf i (1+ (set field-name i)))))

(defun end-log (fstr &rest args)
  "message + time"
  (push
   (apply #'format
          (cons (concat
                 (format-time-string "%H:%M:%S " (apply 'encode-time (butlast (decode-time (current-time)) 3)))
                 fstr)
                args))
   *important-msgs*))

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

(defvar do-not-encrypt '("gpg"))

(defun cloud-connected-p()
  (and
   *cloud-dir* *contents-name*
   (file-readable-p *cloud-dir*)))
;;(file-readable-p (concat *cloud-dir* *contents-name* ".gpg")

(defun write-conf()
(with-temp-file *local-config*
  (insert (format "delete-contents=%s" (if cloud-delete-contents "yes" "no"))) (newline)
  (insert (format "contents-name=%s" *contents-name*)) (newline)
  (insert (format "password=%s" *password*)) (newline)
  (insert (format "number-or-CPU-cores=%s" *Ncores**password)) (newline)
  (insert (format "cloud-directory=%s" *cloud-dir*)) (newline)))

(defun cloud-init() "initializes cloud directory and generates password -- runs only once"
(interactive)
(when (yes-or-no-p "Is cloud mounted?")
(setf *cloud-dir* (read-string "cloud directory=" *cloud-dir*))
(ifn (member (safe-mkdir *cloud-dir*) '(:exists t))
(clog :error "could not create/acess directory %s" *cloud-dir*)

(if (directory-files *cloud-dir* nil "^.\+.gpg$" t)
    (clog :error "please clean the directory %s before asking me to initialize it" *cloud-dir*)
(clog :info "creating (main) contents file in unused directory %s" *cloud-dir*)
(ifn-set ((*contents-name* (new-file-name *cloud-dir*)))
  (clog :error "could not create DB file in the directory %s" *cloud-dir*)

(setf *password* (rand-str 9))

(ifn (member (safe-mkdir *local-dir*) '(:exists t))
(clog :error "could not create/acess directory %s" *local-dir*)
(write-conf)
(clog :info "use M-x cloud-add in the dired to cloud important files and directories" )))))))

(defun write-fileDB (DBname)
  (with-temp-file DBname
(dolist (hostname *clouded-hosts*) (insert (format "%s " hostname)))
(delete-char -1) (newline)

(dolist (action *pending-actions*)
  (insert (format-action action)) (drop *pending-actions* action) (delete-char -1) (newline))

(newline)

(dolist (file-record *file-DB*)
  (insert (format-file file-record)) (newline))
(setf *removed-files* nil)))

(defun clouded(CN) (concat *cloud-dir* CN ".gpg"))

(defun read-fileDB* (DBname)
  "reads content (text) file into the database *file-DB*"
  (find-file DBname) (goto-char (point-min))
(macrolet ((read-line() '(setf str (buffer-substring-no-properties (point) (line-end-position)))))
  (let ((BN (buffer-name)) str)
(needs-set
 ((*clouded-hosts* 
  (split-string (read-line))
  (clog :error "invalid first line in the contents file %s" DBname)))

(unless (member (system-name) *clouded-hosts*) (cloud-host-add))
(forward-line)

(while (< 0 (length (read-line)))
(clog :info "action %s ..." str)
(let ((action (make-vector (length action-fields) nil)))

(dolist (column (list
                 `(:time-stamp . ,i-time)
                 `(:int . ,i-ID)
                 `(:int . ,i-Nargs)))
  (needs ((col-value (begins-with str (car column)) (bad-column "action" (cdr column))))
     (aset action (cdr column) (car col-value))
     (setf str (cdr col-value))))

(dolist (column (list
                 (cons (cons  :string  (aref action i-Nargs)) i-args)
                 `(:strings . ,i-hostnames)))
  (needs ((col-value (begins-with str (car column)) (bad-column "action" (cdr column))))
     (aset action (cdr column) (car col-value)); was (mapcar #'untilda (car col-value))
     (setf str (cdr col-value))))

(let ((AID (format-time-string "%02m/%02d %H:%M:%S" (aref action i-time))))
(clog :info "... will later be referenced as %s" AID)
  (ifn (member (system-name) (aref action i-hostnames))
      (clog :info "this host is unaffected by action %s" AID)
    (when (perform action)
        (clog :debug "sucessfully performed action %s" AID)
      (clog :error " action %s failed, will NOT retry it" AID))

(when (drop (aref action i-hostnames) (system-name))
  (push action *pending-actions*)))))
  (forward-line))

(forward-line)
(let (all Makefile)
(macrolet ((NL () '(push "
" Makefile))
(inl (&rest format-pars) `(progn (push ,(cons 'format format-pars) Makefile) (NL))))
(inl "cloud=%s" *cloud-dir*) (inl "password=%s" *password*)
(inl "gpg=gpg --pinentry-mode loopback --batch --yes
enc=$(gpg) --symmetric --passphrase $(password) -o
dec=$(gpg) --decrypt   --passphrase $(password) -o
")
(while (< 10 (length (read-line)))
(let ((CF (make-vector (length DB-fields) nil)))
  (ifn (string-match "\"\\(.+\\)\"\s+\\([^\s]+\\)\s+\\([^\s]+\\)\s+\\([^\s]+\\)\s+\\([[:digit:]]+\\)\s+\"\\(.+\\)\"" str)
(inl "# Ignoring invalid line %s in %s" str DBname)

(let* ((FN (match-string 1 str)))
  (aset CF plain FN)
  (aset CF cipher (match-string 2 str))
  (aset CF uname (match-string 3 str))

(aset CF gname (match-string 4 str))
  (aset CF modes (string-to-int (match-string 5 str)))
  (let ((mtime-str (match-string 6 str)))
(ifn (string-match "[0-9]\\{4\\}-[0-9][0-9]-[0-9][0-9] [0-9][0-9]:[0-9][0-9]:[0-9][0-9] [[:upper:]]\\{3\\}" mtime-str)
(bad-column "file" 6 mtime-str)
(aset CF mtime (parse-time mtime-str))))

(let ((remote-exists (file-exists-p (clouded (cipher-name CF))))
      (local-exists (or (cloud-locate-FN FN)

(when-let ((LF (get-file-properties FN)))
(aset LF cipher (aref CF cipher))
(push LF *file-DB*)
LF))))

(cond
((not (or local-exists remote-exists))
 (clog :error "forgetting file %s which is marked as clouded but is neither on local disk nor in the cloud" FN)
 (drop *file-DB* CF))
((or
 (and local-exists remote-exists (time< (aref local-exists mtime) (aref CF mtime)))
 (and (not local-exists) remote-exists))
    (push (format " %s" FN) all) (inl "%s: $(cloud)%s.gpg
\t$(dec) $@ $<
\t-chgrp %s $@
\t-chmod %o $@
\t-touch --date=%S $@
" FN (cipher-name CF) (aref CF gname) (aref CF modes) (full-TS (aref CF mtime))))
((or
 (and local-exists remote-exists (time< (aref CF mtime) (aref local-exists mtime)))
 (and local-exists (not remote-exists)))
(push (format " $(cloud)%s.gpg" (cipher-name CF)) all) (inl
(concat "$(cloud)%s.gpg: %s
\t"
(if (member (file-name-extension FN) do-not-encrypt)
"cp $< $@"
"$(enc) $@ $<")
"
") (cipher-name CF) FN)))))))

(forward-line))

(inl "all:%s
\techo \"background (en/de)cryption finished `date +%%T`\" >> %s
\t-rm %s
\t-rmdir %s
"
(apply #'concat all) (concat *cloud-dir* "history")
cloud-lockfile cloud-lockdir)
(write-region (apply #'concat (reverse Makefile)) nil *cloud-mk*)
(chgrp "tmp" *cloud-mk*))))
(kill-buffer BN))))

(defmacro bad-column (cType N &optional str)
(if str
`(clog :error "invalid %dth column in %s line = %s" ,N ,cType ,str)
`(clog :error "invalid %dth column in %s line" ,N ,cType)))

(defun on-current-buffer-save ()
  "attention: this function might be called many times within a couple of seconds!"
  (let ((plain-file (file-chase-links (buffer-file-name))))
(when (and plain-file (stringp plain-file))
  (let ((file-data (cloud-locate-FN plain-file)))
    (when file-data
      (aset file-data mtime (current-time)))))))
(add-hook 'after-save-hook 'on-current-buffer-save)

(defun cloud-sync()
(interactive)
(let ((time-stamp (TS (current-time)))
(mkdir (safe-mkdir cloud-lockdir)))
(cond
((not mkdir) (clog :error "can not create lock directory %s. Is the remote directory monted?" cloud-lockdir))
((member mkdir '(:exists))
       (clog :error "lock directory %s exists; someone else might be syncing right now. If this is not the case, remove %s manually" cloud-lockdir cloud-lockdir))
(t
   (write-region time-stamp nil cloud-lockfile)
   (let ((ok t))
(ifn (cloud-connected-p)
      (clog :error "cloud-sync header failed")
          (clog :info "started syncing")

(if (and *gpg-process* (process-live-p *gpg-process*))
(clog :error "I will not start new (en/de) coding process because the previous one is still funning")
(read-fileDB)
(setf *gpg-process* (apply #'start-process (append (list
"cloud-batch" 
(generate-new-buffer "*cloud-batch*")
"make")
(split-string (format "-j%d -f %s all" *Ncores* *cloud-mk*))))))

(ifn ok (progn
(end-log "error (en/de)crypting files, cloud-sync aborted")
(clog :error "error (en/de)crypting files, cloud-sync aborted"))
(let ((tmp-CCN (concat *local-dir* "CCN")))
   (write-fileDB tmp-CCN)
   (if (setf ok 
(= 0 (apply #'call-process
(append (list "gpg" nil nil nil)
(split-string (format
"--batch --yes --pinentry-mode loopback --passphrase %s  -o %s --symmetric %s"
*password* (clouded *contents-name*)  tmp-CCN))))))
       (when cloud-delete-contents (safe-dired-delete tmp-CCN))
     (clog :error "failed to encrypt content file %s to %s!" tmp-CCN *contents-name*))))

(dolist (msg (reverse *important-msgs*)) (message msg))
(setf *important-msgs* nil)
(clog :info "done syncing")
ok))
(ifn (and (safe-delete-file cloud-lockfile) (safe-delete-dir cloud-lockdir))
     (clog :error "could not delete lock file %s and directory %s" cloud-lockfile cloud-lockdir)
     (write-region (format "%s: %s -- %s
" (system-name) time-stamp (format-time-string "%H:%M:%S" (current-time))) nil (concat *cloud-dir* "history") t))))))

(defvar action-fields '(i-time i-ID i-args i-hostnames i-Nargs))
(let ((i 0)) (dolist (AF action-fields) (setf i (1+ (set AF i)))))

(defvar action-IDs '(i-forget i-delete i-rename i-host-add i-host-forget))
(let ((i 0)) (dolist (AI action-IDs) (setf i (1+ (set AI i)))))

(defun new-action (a-ID &rest args)
  (let ((action (make-vector (length action-fields) nil)))
    (aset action i-ID a-ID)
    (aset action i-time (current-time))
    (aset action i-args args)
    (aset action i-hostnames (remove (system-name) *clouded-hosts*))
    (push action *pending-actions*)))

(defun perform(action)
  (let ((arguments (aref action i-args)))
    (case= (aref action i-ID)
      (i-host-forget (dolist (arg arguments) (drop *clouded-hosts* arg)) t)
      (i-host-add (dolist (arg arguments) (push arg *clouded-hosts*)) t)
      (i-forget (cloud-forget-many arguments) t)
      (i-delete (cloud-rm arguments) t)
      (i-rename (cloud-rename-file (first arguments) (second arguments)) t)
      (otherwise (clog :error "unknown action %d" (aref action i-ID)))))
   (drop *pending-actions* action) t)

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
  (let (failure)

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

(defun cloud-delete-file (local-FN)
  (needs ((DB-rec (cloud-locate-FN local-FN) (clog :info "delete: doing nothing since %s is not clouded")))
    (new-action i-delete local-FN)
    (drop *file-DB* DB-rec)
    (safe-dired-delete (concat *cloud-dir* (aref DB-rec cipher) ".gpg"))))

(defun contained-in(dir-name); dir-name must end with a slash /
    (let (res)
      (dolist (DB-rec *file-DB*)
        (when(and
(< (length dir-name) (length (aref DB-rec plain)))
(string=(substring-no-properties (aref DB-rec plain) 0 (length dir-name)) dir-name))
          (push DB-rec res)))
      res))

(defun add-to-actions(hostname)
  (dolist (action *pending-actions*)
    (unless (member hostname (aref action i-hostnames))
      (aset action i-hostnames (cons hostname (aref action i-hostnames))))))
(defun erase-from-actions(hostname)
  (dolist (action *pending-actions*)
    (when (member hostname (aref action i-hostnames))
      (aset action i-hostnames (remove hostname (aref action i-hostnames))))))

(defun cloud-host-add ()
  "adding THIS host to the cloud sync-system"
(let ((hostname (system-name)))
  (unless (member hostname *clouded-hosts*)
    (push hostname *clouded-hosts*))
  (new-action i-host-add hostname)
  (add-to-actions hostname)))

(defun cloud-host-forget ()
  "remove host from the cloud sync-system"
  (let ((hostname (system-name)))
    (when (yes-or-no-p (format "Forget the host %s?" hostname))
      (new-action i-host-forget hostname)
      (if (cloud-sync)
          (safe-dired-delete *local-config*)
        (clog :error "sync failed, so I will not erase local configuration")))))

(defun cloud-add (&optional FN)
  (interactive)
  (if (string= major-mode "dired-mode")
      (dired-map-over-marks (add-files (dired-get-filename)) nil)
    (unless
        (add-files (read-string "file to be clouded=" (if FN FN "")))
      (clog :error "could not cloud this file"))))

(defun cloud-forget-file (local-FN); called *after* the file has already been sucessfully deleted
   (push local-FN *removed-files*)
  (needs ((DB-rec (cloud-locate-FN local-FN) (clog :info "forget: doing nothing since %s is not clouded" local-FN))
          (cloud-FN (concat  *cloud-dir* (aref DB-rec cipher) ".gpg") (clog :error "in DB entry for %s" local-FN)))
   (drop *file-DB* DB-rec)
   (push local-FN *removed-files*)
   (safe-dired-delete cloud-FN) t))

(defun cloud-forget-recursive(FN); called *after* the file has already been sucessfully deleted
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
      (loop for property in (list mtime modes uname gname) do
            (aset target property (aref source property)))
      (drop *file-DB* source))
     (source (aset source plain new))
     (target (setf target (get-file-properties new))))))

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
(clog :debug "cloud-start: *local-config* = %s" *local-config*)
(if-let ((conf (read-conf *local-config*)))
    (ifn (and
          (if-let ((CD (cdr (assoc "cloud-directory" conf))))
                  (setf *cloud-dir* CD); "/mnt/lws/cloud/"
                  (setf *cloud-dir* (read-string "cloud directory=" *cloud-dir*))
                  (write-conf) t)
(progn
(setf cloud-lockdir (concat *cloud-dir* "now-syncing/"))
(setf cloud-lockfile (concat cloud-lockdir (system-name)))
 (when-let ((delete-contents (cdr (assoc "delete-contents" conf))))
          (setf cloud-delete-contents (if (string= "no" delete-contents) nil t)))t)
          (setf *contents-name* (cdr (assoc "contents-name" conf)))
(setf *Ncores* (or (cdr (assoc "number-or-CPU-cores" conf)) 1))
          (setf *password*  (cdr (assoc "password" conf))))
         (clog :error "cloud-start header failed, consider (re)mounting %s or running (cloud-init)" *cloud-dir*)
         (cloud-sync))
    (clog :warning "could not read local configuration file")
    (when (yes-or-no-p "(Re)create configuration?")
      (cloud-init))))

(defun read-fileDB()
  (let ((tmp-CCN (concat *local-dir* "CCN")))
(or
(and
 (cloud-connected-p)
(= 0 (apply #'call-process
(append (list "gpg" nil nil nil)
(split-string (format
"--batch --yes --pinentry-mode loopback --passphrase %s -o %s --decrypt %s"
*password* tmp-CCN  (clouded *contents-name*))))))
 (progn (read-fileDB* tmp-CCN)
        (if cloud-delete-contents
            (safe-dired-delete tmp-CCN) t)))
(progn (clog :error "cloud-start header failed") nil))))

(defun read-conf (file-name)
  "reads configuration file"
(clog :debug "read-conf")
  (find-file *local-config*) (goto-char (point-min)); opening config file
  (let (res str (BN (buffer-name)))
    (while (and
            (setf str (buffer-substring-no-properties (point) (line-end-position)))
            (< 0 (length str)))
     (if (string-match "^\\(\\ca+\\)=\\(\\ca+\\)$" str)
         (push (cons (match-string 1 str) (match-string 2 str)) res)
       (clog :error "garbage string in configuration file: %s" str))
(forward-line))
(kill-buffer BN)
    res))

(unless (boundp '*loaded*)
  (defvar *loaded* nil)); actually supposed to be defined in ~/.emacs