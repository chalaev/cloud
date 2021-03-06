(require 'dired-aux)
(ert-deftest rename-file()
"renaming file on hostA leads to the same action on hostB"
(let(FN1 BN1 DN1 FN2)
(debug-environment
(host> (car  hostnames) (cloud-start) (clog :info "test rename-file 1> clouded-hosts =%s" (together(debug-clouded-hosts))))
(host> (cadr hostnames) (cloud-start) (clog :info "test rename-file 2> clouded-hosts =%s" (together(debug-clouded-hosts))))
(host> (car  hostnames) (cloud-start)
    (should(file-exists-p (untilde file-1)))
    (setf FN1 (tilde file-1a)
          DN1 (file-name-directory FN1)
          BN1 (file-name-nondirectory FN1)
          FN2 (tilde (concat DN1 "new-" BN1)))
    (clog :info "FN1= %s BN1= %s, FN2= %s" FN1 BN1 FN2)
    (should(= 0 (length (debug-remote-actions))))
    (dired-rename-file (untilde FN1) (untilde FN2) t)
    (should(= 1 (length (debug-remote-actions))))
    (clog :info "rename action: %s" (format-action(car(debug-remote-actions))))
    (cloud-sync))
(clog :info "finished with host %s, switching to %s" (car hostnames) (cadr hostnames))
(host> (cadr hostnames)
    (clog :info "file1= %s" file-1a)
    (should(file-exists-p (untilde file-1a)))
    (should(file-exists-p (untilde FN1)))
    (should(not(file-exists-p (untilde FN2))))
    (cloud-start)
    (should(file-exists-p (untilde FN2)))))))
