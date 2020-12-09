OFNs = cloud 2
ORGs = $(addsuffix .org, $(OFNs))
dependsOn = cl epg dired-aux timezone diary-lib subr-x shalaev
# ← I should try removing "cl" from this list

all: README.md packaged/cloud.el $(addprefix generated/from/, $(ORGs)) test

test: packaged/cloud.el generated/from/cloud.org
	@echo "\nTesting on micro level:\n"
	emacs --no-site-file --batch -l ert  -l "goodies/start.el" --eval "(mapcar #'require '($(dependsOn)))"  -l packaged/cloud.el -l generated/tests/micro.el -f ert-run-tests-batch-and-exit
	@echo "\nTesting on macro level:\n"
	emacs --no-site-file --batch -l ert  -l "goodies/start.el" --eval "(mapcar #'require '($(dependsOn)))"  -l packaged/cloud.el -l generated/tests/macro.el -f ert-run-tests-batch-and-exit
	@echo "\n`date '+%m/%d %H:%M'` TESTS PASSED :)\n"

packaged/cloud.el: version.org generated/from/cloud.org generated/from/2.org packaged/
	sed "s/the-version/`head -n1 $<`/" header.el > $@
	cat 0.el  1.el generated/2.el generated/variables.el generated/functions.el >> $@
	echo "(provide 'cloud)" >> $@
	echo ";;; cloud.el ends here" >> $@
	emacsclient -e '(untilde (cdr (assoc "local-packages" package-archives)))' | xargs cp $@
	-@chgrp tmp $@

version.org: change-log.org helpers/derive-version.el
	emacsclient -e '(progn (load "$(CURDIR)/helpers/derive-version.el") (format-version "$<"))' | sed 's/"//g' > $@
	echo "← generated `date '+%m/%d %H:%M'` from [[file:$<][$<]]" >> $@
	echo "by [[file:helpers/derive-version.el][derive-version.el]]" >> $@
	-@chgrp tmp $@

generated/from/%.org: %.org generated/from/ generated/headers/ generated/tests/
	echo `emacsclient -e '(progn (load "$(CURDIR)/helpers/derive-version.el") (printangle "$<"))'` | sed 's/"//g' > $@
	-@chgrp tmp $@ `cat $@`
	-@chmod a-x `cat $@`

README.md: README.org
	emacsclient -e '(progn (find-file "README.org") (org-md-export-to-markdown) (kill-buffer))'
	@sed -i "s/\.md)/.org)/g"  $@
	-@chgrp tmp $@

clean:
	-rm -r generated version.org

.PHONY: clean quicklisp all git test

%/:
	[ -d $@ ] || mkdir -p $@
