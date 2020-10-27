cloud=/mnt/cloud/
password=*********
gpg=gpg --pinentry-mode loopback --batch --yes
enc=$(gpg) --symmetric --passphrase $(password) -o
dec=$(gpg) --decrypt   --passphrase $(password) -o
localLog=~/.emacs.d/cloud/kalinin.log
date=`date '+%m/%d %T'`

~/.emacs.d/cloud/pass.d/updated:~/.emacs.d/cloud/individual.passes
	awk '{print $2 > $1}' $<
	date > $@
	-chgrp tmp ~/.emacs.d/cloud/pass.d/*

$(cloud)zdJ.gpg: ~/cloud/shell/cloud-git
	$(enc) $@ $<
	-echo "$(date): uploaded ~/cloud/shell/cloud-git" >> $(localLog)

$(cloud)bxj.gpg: ~/elisp-goodies/next-commit.txt
	$(enc) $@ $<
	-echo "$(date): uploaded ~/elisp-goodies/next-commit.txt" >> $(localLog)

$(cloud)LnR.gpg: ~/elisp-goodies/goodies.org
	$(enc) $@ $<
	-echo "$(date): uploaded ~/elisp-goodies/goodies.org" >> $(localLog)

$(cloud)8fj.gpg: ~/cloud/cloud.org
	$(enc) $@ $<
	-echo "$(date): uploaded ~/cloud/cloud.org" >> $(localLog)

all: /mnt/cloud/8fj.gpg /mnt/cloud/LnR.gpg /mnt/cloud/bxj.gpg /mnt/cloud/zdJ.gpg
	echo "background (en/de)cryption on kalinin finished $(date)" >> /mnt/cloud/history
	-rm /mnt/cloud/now-syncing/kalinin
	-rmdir /mnt/cloud/now-syncing/
