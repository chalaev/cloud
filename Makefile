# Time-stamp: <2020-09-07 11:36 EDT> 

non-essential/cloud.el: header.el 0.el goodies/macros.el goodies/functions.el goodies/logging.el 1.el 2.el non-essential/main.el
	cat header.el 0.el goodies/macros.el goodies/functions.el goodies/logging.el 1.el 2.el non-essential/main.el > $@
	echo "(provide 'cloud)" >> $@
	echo ";;; cloud.el ends here" >> $@
	chgrp tmp $@

non-essential/main.el: cloud.org
	emacsclient -e '(org-babel-tangle-file "cloud.org")'
	chgrp tmp $@
	chmod a-x *.el

clean:
	-rm non-essential/*

.PHONY: clean
