cloud=/mnt/lws/cloud/
password="******"
gpg=gpg --pinentry-mode loopback --batch --yes
enc=$(gpg) --symmetric --passphrase $(password) -o
dec=$(gpg) --decrypt   --passphrase $(password) -o
localLog=~/.emacs.d/cloud/kalinin/log
MK=~/.emacs.d/cloud/cloud.mk
date=`date '+%m/%d %T'`

~/.emacs.d/cloud/pass.d/updated: ~/.emacs.d/cloud/individual.passes
	awk '{print $$2 > "/home/shalaev/.emacs.d/cloud/pass.d/"$$1}' $<
	echo $(date) > $@
	-chgrp -R tmp ~/.emacs.d/cloud/pass.d/*

$(cloud)mel.gpg: ~//work/cloud/todo.txt
	@$(enc) $@ $<
	-echo "$(date): uploaded ~//work/cloud/todo.txt" >> $(localLog)

$(cloud)vEV.gpg: ~//work/cloud/README.org
	@$(enc) $@ $<
	-echo "$(date): uploaded ~//work/cloud/README.org" >> $(localLog)

$(cloud)UJT.gpg: ~/.emacs.d/cloud/individual.passes
	@$(enc) $@ $<
	-echo "$(date): uploaded ~/.emacs.d/cloud/individual.passes" >> $(localLog)

$(cloud)ivh.png: ~/20-02.jpg ~/.emacs.d/cloud/pass.d/updated
	convert $< -encipher ~/.emacs.d/cloud/pass.d/ivh $@
	-echo "$(date): uploaded ~/20-02.jpg" >> $(localLog)

/tmp/emacs-cloud.58WR9D: ~/sample-file.gz
	zcat $< > $@

$(cloud)MuC.gpg: /tmp/emacs-cloud.58WR9D
	@$(enc) $@ $<
	rm $<
	-echo "$(date): uploaded ~/sample-file.gz" >> $(localLog)

all: /mnt/lws/cloud/MuC.gpg /mnt/lws/cloud/ivh.png /mnt/lws/cloud/UJT.gpg /mnt/lws/cloud/vEV.gpg /mnt/lws/cloud/mel.gpg
	echo "background (en/de)cryption on kalinin finished $(date)" >> /mnt/lws/cloud/history
	@sed 's/******/******/g' ~/.emacs.d/cloud/cloud.mk > ~/.emacs.d/cloud/cloud.mk.bak

