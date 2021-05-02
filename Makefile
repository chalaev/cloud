OFNs = cloud 2
ORGs = $(addsuffix .org, $(OFNs))
export EMACS = emacs -q --no-site-file --batch

packaged/cloud.el: version.org packaged/ generated/cloud.el generated/from/cloud.org generated/from/2.org tests/micro.log tests/meso.log tests/macro-0.log tests/macro-1.log tests/macro-2.log shell/indices.sh
	sed '/^;;test>;;/d' generated/cloud.el > $@
	emacsclient -e '(untilde (cdr (assoc "local-packages" package-archives)))' | xargs cp $@
	-@chgrp tmp $@

shell/indices.sh: generated/from/cloud.org
	echo "# auto-generated from cloud.org"  > $@
	sed -e 's/"//g' -e "s/-//g" -e "s/ /\n/g" generated/indices.sh >> $@

generated/cloud.el: version.org packaged/ generated/from/cloud.org generated/from/2.org
	sed "s/the-version/`head -n1 $<`/" header.el > $@
	cat generated/main-0.el generated/indices.el generated/main-1.el 0.el 1.el generated/2.el generated/3.el generated/main-2.el >> $@
	echo "(provide 'cloud)" >> $@
	echo ";; cloud.el ends here" >> $@
	-@chgrp tmp $@

tests/cloud.el: generated/cloud.el
	sed 's/^;;test>;;//' $< > $@
	-@chgrp tmp $@

tests/%.log: generated/from/testing.org generated/from/debug.org tests/cloud.el tests/common.conf
	@echo "\n-= Testing at $(patsubst %.log,%,$@) scale =-\n"
	$(EMACS) --eval '(defvar debug-make-dir "$(CURDIR)")' -l ~/.emacs.d/start.el -l tests/common.el -l $(patsubst %.log,%.el,$@) -f ert-run-tests-batch-and-exit 2> $@
	@echo "\n`date '+%m/%d %H:%M'` TEST PASSED :) -- see $@\n"
	-@chgrp tmp $@

tests/common.conf: generated/from/testing.org
	$(EMACS) --eval '(defvar debug-make-dir "$(CURDIR)")' -l ~/.emacs.d/start.el -l tests/prepare.el 2> tests/prepare.log
	-@chgrp tmp tests/prepare.log

version.org: change-log.org
	emacsclient -e "(progn (require 'version) (format-version \"$<\"))" | sed 's/"//g' > $@
	@echo "← generated `date '+%m/%d %H:%M'` from [[file:$<][$<]]" >> $@
	@echo "by [[https://github.com/chalaev/lisp-goodies/blob/master/packaged/version.el][version.el]]" >> $@
	-@chgrp tmp $@

generated/from/%.org: %.org generated/from/ generated/headers/ tests/
	@echo "\nNow emacs is probably waiting for your responce..."
	emacsclient -e "(progn (require 'version) (printangle \"$<\"))" | sed 's/"//g' > $@
	-@chgrp tmp $@ `cat $@`
	-@chmod a-x `cat $@`

README.md: README.org
	emacsclient -e '(progn (find-file "README.org") (org-md-export-to-markdown) (kill-buffer))'
	@sed -i "s/\.md)/.org)/g"  $@
	-@chgrp tmp $@

clean:
	-rm -r tests generated version.org packaged

.PHONY: clean all

%/:
	[ -d $@ ] || mkdir -p $@
