(ert-deftest double-rename()
  "rename the file twice"
(let(FN0 FN1 FN2)
(debug-environment
(host> (car  hostnames) (cloud-start) (clog :info "test rename-file 1> clouded-hosts =%s" (together(debug-clouded-hosts))))
(host> (cadr hostnames) (cloud-start) (clog :info "test rename-file 2> clouded-hosts =%s" (together(debug-clouded-hosts))))
(host> (car  hostnames) (cloud-start)
(setf FN0 file-1a
      FN1 (concat file-1a "-1")
      FN2 (concat file-1a "-2"))
    (should(file-exists-p (untilde FN0)))
    (should(not(file-exists-p (untilde FN1))))
    (should(not(file-exists-p (untilde FN2))))
    (should(= 0 (length (debug-remote-actions))))

    (dired-rename-file (untilde FN0) (untilde FN1) t)
    (should(not(file-exists-p (untilde FN0))))
    (should(file-exists-p (untilde FN1)))
    (should(not(file-exists-p (untilde FN2))))
    (should(= 1 (length (debug-remote-actions))))

    (dired-rename-file (untilde FN1) (untilde FN2) t)
    (should(not(file-exists-p (untilde FN0))))
    (should(not(file-exists-p (untilde FN1))))
    (should(file-exists-p (untilde FN2)))
    (should(= 2 (length (debug-remote-actions))))

    (clog :info "test double-rename> rename actions: %s"
(mapconcat #'format-action (debug-remote-actions) "
"))
    (cloud-sync))
(host> (cadr hostnames)
    (should(file-exists-p (untilde FN0)))
    (should(not(file-exists-p (untilde FN1))))
    (should(not(file-exists-p (untilde FN2))))
    (cloud-start)
    (should(file-exists-p (untilde FN2)))
    (should(not(file-exists-p (untilde FN1))))
    (should(not(file-exists-p (untilde FN0))))))))
