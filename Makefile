OFNs = cloud 2
ORGs = $(addsuffix .org, $(OFNs))

all: README.md generated/cloud.tbz $(addprefix generated/from/, $(ORGs)) git

generated/cloud.tbz: generated/from/cloud.org generated/from/2.org version.org
	echo ";; -*- mode: Emacs-Lisp;  lexical-binding: t; -*-" > generated/cloud.el
	cat goodies/cl.el goodies/macros.el goodies/functions.el goodies/file-functions.el \
0.el  1.el generated/2.el \
generated/variables.el generated/functions.el >> generated/cloud.el
	@echo "Testing before we package it:"
	emacs --no-site-file --batch -l ert --eval "(mapcar #'require '(cl epg dired-aux timezone diary-lib subr-x))"  -l generated/cloud.el -l generated/tests.el -f ert-run-tests-batch-and-exit
	@echo "`date '+%m/%d %H:%M'` TESTS PASSED :)\n"
	tar jcfv $@ --transform s/^generated/cloud/ generated/cloud.el
	-@chgrp tmp $@

version.org: change-log.org helpers/derive-version.el
	emacsclient -e '(progn (load "$(CURDIR)/helpers/derive-version.el") (format-version "$<"))' | sed 's/"//g' > $@
	echo "← generated `date '+%m/%d %H:%M'` from [[file:$<][$<]]" >> $@
	echo "by [[file:helpers/derive-version.el][derive-version.el]]" >> $@
	-@chgrp tmp $@

generated/from/%.org: %.org generated/from/ generated/headers/
	@echo `emacsclient -e '(progn (load "$(CURDIR)/helpers/derive-version.el") (printangle "$<"))'` | sed 's/"//g' > $@
	-@chgrp tmp $@ `cat $@`
	-@chmod a-x `cat $@`

README.md: README.org
	emacsclient -e '(progn (find-file "README.org") (org-md-export-to-markdown) (kill-buffer))'
	@sed -i "s/\.md)/.org)/g"  $@
	-@chgrp tmp $@

clean:
	-rm -r generated version.org

.PHONY: clean quicklisp all git

%/:
	[ -d $@ ] || mkdir -p $@
