;; -*-  mode: Emacs-Lisp; lexical-binding: t; -*-
;; These are "microscopic" tests, on microscopic scale: they test single functions in a stabdard environment
;; If a "microscopic" test fails, it generally makes no sence to run larger-scale (mesoscopic and mesoscopic) tests.
(require 'ert)

(ert-deftest format-conf()
(let ((remote-directory "/mnt/my-cloud/")
      (number-of-CPU-cores 123)
      (ignored-dirs '("/abc/" "/def/")))
(should (string= "remote-directory=/mnt/my-cloud/"   (format-conf "remote-directory")))
(should (string= "number-of-CPU-cores=123" (format-conf "number-of-CPU-cores")))
(should (string= "ignored-dirs=/abc/ /def/ " (format-conf "ignored-dirs")))))

(let ((general-FR ["~/pam.d/xscreensaver" "qwe" (24506 18567 0 0) 416 41 "shalaev"])
      (gzipped ["~/shalaev.1.obsolete.gz" "4R6" (21621 47298 0 0) 416 41 "shalaev"])
      (encrypted ["~/big-secret.gpg" "sDF" (21621 47298 0 0) 416 41 "shalaev"])
      (image ["~/photo.jpeg" "rd2" (21621 47298 0 0) 416 41 "shalaev"]))

(defun make-temp-file (FN) (concat "/tmp/" FN "bZIZVA"))

(ert-deftest enc-make-stanza()
(skip-unless (eql system-type 'gnu/linux))

(should (string= (enc-make-stanza general-FR)
"
$(cloud)qwe.gpg: ~/pam.d/xscreensaver
\t@$(enc) $@ $<
\t-@echo \"$$(date): uploaded $<\" >> $(localLog)
"))

(should (string= (enc-make-stanza gzipped)
"
/tmp/emacs-cloud.bZIZVA: ~/shalaev.1.obsolete.gz
	zcat $< > $@

$(cloud)4R6.gpg: /tmp/emacs-cloud.bZIZVA
\t@$(enc) $@ $<
	rm $<
\t-@echo \"$$(date): uploaded $<\" >> $(localLog)
"))

(should (string= (enc-make-stanza encrypted)
"
$(cloud)sDF.gpg: ~/big-secret.gpg
\tcp $< $@
\t-@echo \"$$(date): uploaded $<\" >> $(localLog)
"))

(should (string= (enc-make-stanza image)
"
$(cloud)rd2.png: ~/photo.jpeg ~/.emacs.d/cloud/pass.d/updated
\tconvert $< -encipher ~/.emacs.d/cloud/pass.d/rd2 $@
\t-@echo \"$$(date): uploaded $<\" >> $(localLog)
")))

(ert-deftest dec-make-stanza()
(skip-unless (eql system-type 'gnu/linux))
(should (string= (dec-make-stanza general-FR)
"
~/pam.d/xscreensaver: $(cloud)qwe.gpg
\t@$(dec) $@ $<
\t-chgrp shalaev $@
\t-chmod 640 $@
\t-touch --date=\"2020-11-22 06:16:23 EST\" $@
\t-@echo \"$$(date): downloaded $@\" >> $(localLog)
"))

(should (string= (dec-make-stanza gzipped)
"
/tmp/emacs-cloud.bZIZVA:$(cloud)4R6.gpg
\t@$(dec) $@ $<

~/shalaev.1.obsolete.gz: /tmp/emacs-cloud.bZIZVA
\tcat $< | gzip > $@
	rm $<
\t-chgrp shalaev $@
\t-chmod 640 $@
\t-touch --date=\"2014-11-26 06:25:54 EST\" $@
\t-@echo \"$$(date): downloaded $@\" >> $(localLog)
"))

(should (string= (dec-make-stanza encrypted)
"
~/big-secret.gpg: $(cloud)sDF.gpg
\tcp $< $@
\t-chgrp shalaev $@
\t-chmod 640 $@
\t-touch --date=\"2014-11-26 06:25:54 EST\" $@
\t-@echo \"$$(date): downloaded $@\" >> $(localLog)
"))

(should (string= (dec-make-stanza image)
"
~/photo.jpeg: $(cloud)rd2.png  ~/.emacs.d/cloud/pass.d/updated
\tconvert $< -decipher ~/.emacs.d/cloud/pass.d/rd2 $@
\t-chgrp shalaev $@
\t-chmod 640 $@
\t-touch --date=\"2014-11-26 06:25:54 EST\" $@
\t-@echo \"$$(date): downloaded $@\" >> $(localLog)
"))))
(ert-deftest if-let-key.1()
  (should (eql :yes (if-let-key #'identity ((a "a") (b (concat a "b")))
		      :yes
		      (when (boundp 'b) (setf b (concat b "a")))
		      (setf a (concat a "a"))
		      (if (boundp 'b) (concat a b) a))))
  (should (eql :nil (if-let-key #'car ((a nil) (b (concat a "b")))
		      :yes
		      (when (boundp 'b) (setf b (concat b "a")))
		      :nil)))
  (should (= 3 (if-let-key #'car ((a '(1 . 2)) (b '(nil . 3)))
		 :yes
		 (cdr b)))))

(ert-deftest if-let-key.2()
  (should (= 1 (ifn-let-key #'car ((r1 '(nil . :bad))) 1 2))))

(ert-deftest if-failed()
(clog :info "IGNORE the following :error log message which is part of a test:")
(let* ((ok t) (IFFA (if-failed '(nil . "it's too bad!") "tutto cazzo" 1)))
  (should  (not ok))
  (should (not (car IFFA)))
  (should (string= "tutto cazzo
because it's too bad!" (cdr IFFA))))
(clog :info "IGNORE the following :error log message which is part of a test:")
(let* ((ok t) (IFFA (if-failed '(nil . "there is a big problem") '("could not encrypt %s to %s" "aaa.txt" "bbb.gpg") 2)))
  (should (not ok))
  (should (not (car IFFA)))
  (should (string= "could not encrypt aaa.txt to bbb.gpg
because there is a big problem" (cdr IFFA))))
(let* ((ok t))
(should (and ok (= 3 (if-failed '(t . "there is a big problem") '("could not encrypt %s to %s" "aaa.txt" "bbb.gpg") 3))))))
