
README.md: generated/cloud.el README.org
	emacsclient -e '(progn (find-file "README.org") (org-md-export-to-markdown) (kill-buffer))'
	sed -i "s/\.md)/.org)/g"  $@
	-chgrp tmp $@

generated/cloud.el: header.el 0.el goodies/macros.el goodies/functions.el goodies/file-functions.el goodies/logging.el 1.el generated/2.el generated/main.el
	echo ";; -*- mode: Emacs-Lisp;  lexical-binding: t; -*-" >  $@
	cat header.el 0.el goodies/macros.el goodies/functions.el goodies/file-functions.el goodies/logging.el 1.el generated/2.el generated/main.el >> $@
	echo "(provide 'cloud)" >> $@
	echo ";; cloud.el ends here" >> $@
	-chgrp tmp $@
	-chmod a-x generated/*.el

generated/2.el: 2.org
	emacsclient -e '(org-babel-tangle-file "$<")'
	-chgrp tmp $@
	-chmod a-x $@

generated/main.el: cloud.org
	emacsclient -e '(org-babel-tangle-file "$<")'
	-chgrp tmp $@
	-chmod a-x generated/*.el

clean:
	-rm README.md generated/*

.PHONY: clean
