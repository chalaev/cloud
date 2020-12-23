OFNs = cloud 2
ORGs = $(addsuffix .org, $(OFNs))
EMACS = emacs -q --no-site-file --batch

all: README.md $(addprefix generated/from/, $(ORGs)) test

test: micro meso macro

micro: packaged/cloud.el generated/from/cloud.org generated/tests/micro.el
	@echo "\n-= Testing on MICRO scale: =-\n"
	$(EMACS) -l goodies/start.el -l $< -l generated/tests/micro.el -f ert-run-tests-batch-and-exit
	@echo "\n`date '+%m/%d %H:%M'` MICRO TESTS PASSED :)\n"

macro: packaged/cloud.el generated/from/cloud.org generated/tests/meso.el generated/tests/macro.el
	@echo "\n-= Testing on MACRO scale: =-\n"
	$(EMACS) -l goodies/start.el -l $< -l generated/tests/macro.el -f ert-run-tests-batch-and-exit
	@echo "\n`date '+%m/%d %H:%M'` MACRO TESTS PASSED :)\n"

meso: packaged/cloud.el generated/from/cloud.org generated/tests/meso.el
	@echo "\n-= Testing on MESO scale: =-\n"
	$(EMACS) -l goodies/start.el -l $< -l generated/tests/meso.el  -f ert-run-tests-batch-and-exit
	@echo "\n`date '+%m/%d %H:%M'` MESO TESTS PASSED :)\n"

generated/tests/micro.el: generated/from/cloud.org generated/from/2.org
	cat generated/micro.el generated/micro-2.el > $@
	-@chgrp tmp $@
	-@chmod a-x $@

generated/tests/meso.el: generated/from/testing.org
	cat generated/headers/tests.el generated/headers/meso.el generated/meso-0.el generated/meso.el > $@
	-@chgrp tmp $@
	-@chmod a-x $@

generated/tests/macro.el: generated/from/testing.org
	cat generated/headers/tests.el generated/headers/meso.el generated/macro.el > $@
	-@chgrp tmp $@
	-@chmod a-x $@

packaged/cloud.el: version.org generated/from/cloud.org generated/from/2.org packaged/
	sed "s/the-version/`head -n1 $<`/" header.el > $@
	cat 0.el 1.el generated/2.el generated/variables.el generated/functions.el >> $@
	echo "(provide 'cloud)" >> $@
	echo ";;; cloud.el ends here" >> $@
	emacsclient -e '(untilde (cdr (assoc "local-packages" package-archives)))' | xargs cp $@
	-@chgrp tmp $@

version.org: change-log.org helpers/derive-version.el
	emacsclient -e '(progn (load "$(CURDIR)/helpers/derive-version.el") (format-version "$<"))' | xargs echo > $@
	echo "← generated `date '+%m/%d %H:%M'` from [[file:$<][$<]]" >> $@
	echo "by [[file:helpers/derive-version.el][derive-version.el]]" >> $@
	-@chgrp tmp $@

generated/from/%.org: %.org generated/from/ generated/headers/ generated/tests/
	emacsclient -e '(progn (load "$(CURDIR)/helpers/derive-version.el") (printangle "$<"))' | xargs echo > $@
	-@chgrp tmp $@ `cat $@`
	-@chmod a-x `cat $@`

README.md: README.org
	emacsclient -e '(progn (find-file "README.org") (org-md-export-to-markdown) (kill-buffer))'
	@sed -i "s/\.md)/.org)/g"  $@
	-@chgrp tmp $@

clean:
	-rm -r generated version.org

.PHONY: clean quicklisp all git test meso micro macro

%/:
	[ -d $@ ] || mkdir -p $@
