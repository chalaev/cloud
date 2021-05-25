(ert-deftest rename-directory-of-changed-file-1()
  "rename existing directory together with its content"
;; :expected-result :failed
(let(FN0 DN0 BN0 BN1)
(debug-environment
(host> (car  hostnames) (cloud-start))
(host> (cadr hostnames) (cloud-start))
(host> (car  hostnames) (cloud-start)
    (setf FN0 (tilde file-in-dir-1a)
          DN0 (tilde(directory-file-name(file-name-directory FN0)))
          DN1 (concat DN0 ".1")
          FN1 (FN DN1 (file-name-nondirectory FN0)))
    (should(file-exists-p (untilde FN0)))
    (should(= 0 (length (debug-remote-actions))))
    (cloud-add (echo-to-file FN0 "first we change this file, then we rename its directory"))
    (clog :debug "test rename-directory-of-changed-file-1> %s --> %s" DN0 DN1)
    (dired-rename-file (untilde DN0) (untilde DN1) t)
    (should(= 1 (length (debug-remote-actions))))
    (should(file-exists-p (untilde FN1)))
    (clog :info "test rename-directory-of-changed-file-1> rename actions: %s"
(mapconcat #'format-action (debug-remote-actions) "
"))
    (cloud-sync))
(host> (cadr hostnames)
    (should(file-exists-p(untilde FN0)))
    (should(file-exists-p(untilde DN0)))
    (should(not(file-exists-p(untilde DN1))))
    (should(not(file-exists-p(untilde FN1))))
    (cloud-start)
    (should(not(file-exists-p(untilde FN0))))
    (should(not(file-exists-p(untilde DN0))))
    (should(file-exists-p(untilde DN1)))
    (should(file-exists-p(untilde FN1)))))))