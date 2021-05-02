(ert-deftest cloud-sync-1()
"copying one file from hostA to hostB"
(debug-environment; ← defines variables: hostnames remote-directory remote/files password
(let(FN file-content)
(host> (car hostnames)
(clog :debug "checking that %s exists on host A:" (setf FN file-1))
(should (file-exists-p (untilde FN)))
(setf file-content (cat-file(untilde file-1)))
(cloud-start)
(cloud-add file-1)
(cloud-sync))
(clog :info "finished with host %s, switching to %s" (car hostnames) (cadr hostnames))
(host> (cadr hostnames)
(clog :debug "checking that %s DOES NOT exist on %s BEFORE SYNCING:" FN (cadr hostnames))
(should(not (file-exists-p (untilde FN))))
(cloud-start)
(should(file-exists-p (untilde FN))); so now file-1 exists on-hostB
(should(string= file-content (cat-file (untilde FN))))))))

(ert-deftest cloud-sync-2()
"copying one file from hostA to hostB"
(debug-environment
(let(file-content)
(host> (car hostnames)
  (setf file-content (cat-file (untilde file-1a)))
  (cloud-start)
  (clog :info "touch 'now + 5 sec' %s" file-1a) 
  (set-file-times (untilde file-1a) (time-add (current-time) 5))
  (cloud-add file-1a)
  (cloud-sync))
(host> (cadr hostnames)
  (should(file-exists-p (untilde file-1a)))
  (should(not(string= file-content (cat-file (untilde file-1a))))); files file-1a on two hostnames have the same name, but different content
  (cloud-start); cloud-start calls cloud-sync that has downloaded file-1a from remote directory
  (should(string= file-content (cat-file (untilde file-1a))))))))
