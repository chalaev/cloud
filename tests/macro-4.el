(ert-deftest rename-directory-and-files()
  "rename existing directory together with its content"
(let(DN0 BN0 DN1 BN1)
(debug-environment
(host> (car  hostnames) (cloud-start))
(host> (cadr hostnames) (cloud-start))
(host> (car  hostnames) (cloud-start)
    (setf DN0 (tilde(directory-file-name dir-1a))
          BN0 (file-name-nondirectory file-in-dir-1a)
          DN1 (tilde(concat DN0 ".1"))
          BN1 (concat BN0 ".1"))
    (should(file-exists-p (untilde DN0)))
    (should(file-exists-p (untilde(untilde(FN DN0 BN0)))))
    (should(= 0 (length (debug-remote-actions))))

    (clog :debug "test rename-directory-and-files> %s --> %s" DN0 DN1); first we rename the directory
    (dired-rename-file (untilde DN0) (untilde DN1) t)
    (should(= 1 (length (debug-remote-actions))))
    (should(file-exists-p (untilde DN1)))
    (should(file-exists-p (untilde(FN DN1 BN0))))

    (clog :debug "test rename-directory-and-files> %s --> %s" (FN DN1 BN0) (FN DN1 BN1))
    (dired-rename-file (untilde(FN DN1 BN0)) (untilde(FN DN1 BN1)) t); and then the file inside it
    (should(= 2 (length (debug-remote-actions))))
    (should(file-exists-p (untilde(FN DN1 BN1))))

    (clog :info "test rename-directory-and-files> rename actions: %s"
(mapconcat #'format-action (debug-remote-actions) "
"))
    (cloud-sync))
(host> (cadr hostnames)
    (should(file-exists-p(untilde dir-1a)))
    (should(file-exists-p(untilde file-in-dir-1a)))
    (cloud-start)
    (should(file-exists-p(untilde (FN DN1 BN1))))))))
