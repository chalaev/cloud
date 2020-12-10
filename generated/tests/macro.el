;; -*-  mode: Emacs-Lisp; lexical-binding: t; -*-
(defmacro first-run(&rest body)
"simulating first run on a new host -- in order to test config file creation"
(let ((res (s-gensym "r")))
`(let* ((emacs-d (file-name-as-directory (make-temp-file "cloud-test." t)))
        (remote-directory (file-name-as-directory (make-temp-file "cloud-mnt-remote." t))))
(clog :debug "remote-directory=%s" remote-directory)
(clog :debug "local-dir=%s" (local-dir))
(unless (ensure-dir-exists (local-dir)) (clog :error "fatal: cannot create %s" (local-dir)))
(clog :debug "ensure-dir-exists(%s)" (local/host/))
(ifn (ensure-dir-exists (local/host/)) (clog :error "cannot create %s" (local/host/))
(clog :debug "Starting new test environment in the directory %s" emacs-d)
(let ((,res (progn ,@body)))
(safe-delete-dir emacs-d)
(safe-delete-dir remote-directory)
,res)))))

(ert-deftest cloud-init()
   "just check that non-empty config file is created during the first run"
  (should 
(first-run
  (cloud-init remote-directory) 
(when-let ((FR (get-file-properties (concat emacs-d "cloud/" localhost "/config")))
           (FSize (aref (get-file-properties (concat emacs-d "cloud/" localhost "/config")) size)))
    (clog :debug "deftest cloud-init: config file size = %d bytes" FSize)
(< 100 FSize)))))
