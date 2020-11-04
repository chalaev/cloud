cloud=/mnt/lws/cloud/
password=*********
gpg=gpg --pinentry-mode loopback --batch --yes
enc=$(gpg) --symmetric --passphrase $(password) -o
dec=$(gpg) --decrypt   --passphrase $(password) -o
localLog=~/.emacs.d/cloud/kolmogorov.log
MK=~/.emacs.d/cloud/cloud.mk
date=`date '+%m/%d %T'`

~/.emacs.d/cloud/pass.d/updated: ~/.emacs.d/cloud/individual.passes
	awk '{print $$2 > "/home/shalaev/.emacs.d/cloud/pass.d/"$$1}' $<
	echo $(date) > $@
	-chgrp -R tmp ~/.emacs.d/cloud/pass.d/*

$(cloud)UJT.gpg: ~/.emacs.d/cloud/individual.passes
	$(enc) $@ $<
	-echo "$(date): uploaded ~/.emacs.d/cloud/individual.passes" >> $(localLog)

$(cloud)plx.gpg: ~/learn/shell/3.sh
	$(enc) $@ $<
	-echo "$(date): uploaded ~/learn/shell/3.sh" >> $(localLog)

all: /mnt/lws/cloud/plx.gpg /mnt/lws/cloud/UJT.gpg
	echo "background (en/de)cryption on kolmogorov finished $(date)" >> /mnt/lws/cloud/history
	-rm /mnt/lws/cloud/now-syncing/kolmogorov
	-rmdir /mnt/lws/cloud/now-syncing/
	-cat $(MK) > $(MK).previous
	-chgrp tmp $(MK) $(MK).previous
	-emacsclient -e '(reset-Makefile)'
