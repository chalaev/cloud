(ert-deftest rename-directory()
  "same as rename-file, but for directories"
(let(FN0 DN1 FN1)
(debug-environment
(host> (car  hostnames) (cloud-start))
(host> (cadr hostnames) (cloud-start))
(host> (car  hostnames) (cloud-start)
    (setf FN0 (tilde(directory-file-name dir-1a))); without final slash; dir-1a has the same name on both hosts
    (should (file-exists-p (untilde FN0)))
    (setf FN1 (tilde(concat FN0 ".1"))); new directory name
    (should(= 0 (length (debug-remote-actions))))
    (clog :debug "test rename-directory> dired-rename-file %s --> %s" FN0 FN1)
    (dired-rename-file FN0 FN1 t)
    (should (file-exists-p (untilde FN1)))
    (should(= 1 (length (debug-remote-actions))))
    (clog :info "test rename-directory> rename actions: %s"
(mapconcat #'format-action (debug-remote-actions) "
"))
    (cloud-sync))
(clog :info "finished with host %s, switching to %s" (car hostnames) (cadr hostnames))
(host> (cadr hostnames)
    (should(file-exists-p (untilde FN0)))
    (should(not(file-exists-p (untilde FN1))))
    (cloud-start)
    (should(not(file-exists-p (untilde FN0))))
    (should(file-exists-p (untilde FN1)))))))
