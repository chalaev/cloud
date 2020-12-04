;; -*- mode: Emacs-Lisp;  lexical-binding: t; -*-
(ert-deftest format-conf()
(let ((numerical-parameters '("number-of-CPU-cores"))
	(lists-of-strings '("junk-extensions" "ignored-dirs"))
	(remote-directory "/mnt/my-cloud/")
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
	$(enc) $@ $<
	-@echo \"$$(date): uploaded $<\" >> $(localLog)
"))

(should (string= (enc-make-stanza gzipped)
"
/tmp/emacs-cloud.bZIZVA: ~/shalaev.1.obsolete.gz
	zcat $< > $@

$(cloud)4R6.gpg: /tmp/emacs-cloud.bZIZVA
	$(enc) $@ $<
	rm $<
	-@echo \"$$(date): uploaded $<\" >> $(localLog)
"))

(should (string= (enc-make-stanza encrypted)
"
$(cloud)sDF.gpg: ~/big-secret.gpg
	cp $< $@
	-@echo \"$$(date): uploaded $<\" >> $(localLog)
"))

(should (string= (enc-make-stanza image)
"
$(cloud)rd2.png: ~/photo.jpeg ~/.emacs.d/cloud/pass.d/updated
	convert $< -encipher ~/.emacs.d/cloud/pass.d/rd2 $@
	-@echo \"$$(date): uploaded $<\" >> $(localLog)
")))

(ert-deftest dec-make-stanza()
(skip-unless (eql system-type 'gnu/linux))
(should (string= (dec-make-stanza general-FR)
"
~/pam.d/xscreensaver: $(cloud)qwe.gpg
	$(dec) $@ $<
	-chgrp shalaev $@
	-chmod 640 $@
	-touch --date=\"2020-11-22 06:16:23 EST\" $@
	-@echo \"$$(date): downloaded $@\" >> $(localLog)

"))

(should (string= (dec-make-stanza gzipped)
"
/tmp/emacs-cloud.bZIZVA:$(cloud)4R6.gpg
	$(dec) $@ $<

~/shalaev.1.obsolete.gz: /tmp/emacs-cloud.bZIZVA
	cat $< | gzip > $@
	rm $<
	-chgrp shalaev $@
	-chmod 640 $@
	-touch --date=\"2014-11-26 06:25:54 EST\" $@
	-@echo \"$$(date): downloaded $@\" >> $(localLog)

"))

(should (string= (dec-make-stanza encrypted)
"
~/big-secret.gpg: $(cloud)sDF.gpg
	cp $< $@
	-chgrp shalaev $@
	-chmod 640 $@
	-touch --date=\"2014-11-26 06:25:54 EST\" $@
	-@echo \"$$(date): downloaded $@\" >> $(localLog)

"))

(should (string= (dec-make-stanza image)
"
~/photo.jpeg: $(cloud)rd2.png  ~/.emacs.d/cloud/pass.d/updated
	convert $< -decipher ~/.emacs.d/cloud/pass.d/rd2 $@
	-chgrp shalaev $@
	-chmod 640 $@
	-touch --date=\"2014-11-26 06:25:54 EST\" $@
	-@echo \"$$(date): downloaded $@\" >> $(localLog)

"))))
