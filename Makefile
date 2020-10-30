# Time-stamp: <2020-09-07 11:36 EDT> 

README.md: generated/cloud.el README.org
	emacsclient -e '(progn (find-file "README.org") (org-md-export-to-markdown))'
	sed -i "s/\.md)/.org)/g"  $@
	chgrp tmp $@

generated/cloud.el: header.el 0.el goodies/macros.el goodies/functions.el goodies/logging.el 1.el 2.el generated/main.el
	echo ";;; -*- mode: Emacs-Lisp;  lexical-binding: t; -*-" >  $@
	cat header.el 0.el goodies/macros.el goodies/functions.el goodies/logging.el 1.el 2.el generated/main.el >> $@
	echo "(provide 'cloud)" >> $@
	echo ";;; cloud.el ends here" >> $@
	chgrp tmp $@
	chmod a-x generated/*.el

generated/main.el: cloud.org
	emacsclient -e '(org-babel-tangle-file "cloud.org")'
	chgrp tmp $@
	chmod a-x generated/*.el

clean:
	-rm generated/*

.PHONY: clean
