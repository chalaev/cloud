;; -*-  lexical-binding: t; -*-
;; tests/micro.el
(mapcar #'(lambda(FN)(load(FN debug-make-dir "tests" FN))) (split-string "common.el"))

(defun make-temp-file (FN &optional dir-flag suffix text)
"locally redefining standard function"
 (concat "/tmp/" FN "bZIZVA"))

(ert-deftest enc-make-stanza()
(skip-unless (eql system-type 'gnu/linux))
;; (setf lexical-binding t) ← does not help
(debug-environment (host> (car hostnames)
(let((general-FR ["~/pam.d/xscreensaver" "shalaev" "shalaev" (24506 18567 0 0) 41 416 "qwe"]); +
     (gzipped ["~/shalaev.1.obsolete.gz" "shalaev" "shalaev" (21621 47298 0 0) 41 416 "4R6" ])
     (encrypted ["~/big-secret.gpg" "shalaev" "shalaev" (21621 47298 0 0) 41 416  "sDF"])
     (image ["~/photo.jpeg" "shalaev" "shalaev" (21621 47298 0 0) 41 416 "rd2"]))

(should (string= (enc-make-stanza general-FR)
"
$(cloud)qwe.gpg: $(HD)pam.d/xscreensaver
\t@$(enc) $@ $<
\t-@echo \"$$(date): uploaded $<\" >> $(localLog)
"))

(should (string= (enc-make-stanza gzipped)
"
/tmp/emacs-cloud.bZIZVA: $(HD)shalaev.1.obsolete.gz
	zcat $< > $@

$(cloud)4R6.gpg: /tmp/emacs-cloud.bZIZVA
\t@$(enc) $@ $<
	rm $<
\t-@echo \"$$(date): uploaded $<\" >> $(localLog)
"))

(should (string= (enc-make-stanza encrypted)
"
$(cloud)sDF.gpg: $(HD)big-secret.gpg
\tcp $< $@
\t-@echo \"$$(date): uploaded $<\" >> $(localLog)
"))

(should (string= (enc-make-stanza image)
"
$(cloud)rd2.png: $(HD)photo.jpeg $(HD).emacs.d/conf/cloud/pass.d/updated
\tconvert $< -encipher $(HD).emacs.d/conf/cloud/pass.d/rd2 $@
\t-@echo \"$$(date): uploaded $<\" >> $(localLog)
"))))))
