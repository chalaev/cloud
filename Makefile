# Time-stamp: <2020-09-07 11:36 EDT> 

cloud.el: header.el 0.el macros.el functions.el logging.el 1.el 2.el main.el
	cat header.el 0.el macros.el functions.el logging.el 1.el 2.el main.el > $@
	echo ";;; cloud.el ends here" >> $@
	chgrp tmp $@

main.el: cloud.org
	emacsclient -e '(org-babel-tangle-file "cloud.org")'
	chgrp tmp $@
	chmod a-x *.el
