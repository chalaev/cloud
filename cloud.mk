cloud=/mnt/lws/cloud/
password=*********
gpg=gpg --pinentry-mode loopback --batch --yes --passphrase $(password)
enc=$(gpg) --symmetric -o
dec=$(gpg) --decrypt   -o

$(cloud)bxj.gpg: ~/elisp-goodies/next-commit.txt
	$(enc) $@ $<

$(cloud)G2B.gpg: ~/.emacs.d/bookmarks
	$(enc) $@ $<

$(cloud)ypc.gpg: ~/DE/rechtschreibung.tex
	$(enc) $@ $<

$(cloud)boz.gpg: ~/DE/buch.txt
	$(enc) $@ $<

$(cloud)MxX.gpg: ~/secrets.txt.gpg
	cp $< $@

all: $(cloud)MxX.gpg $(cloud)boz.gpg $(cloud)ypc.gpg $(cloud)G2B.gpg $(cloud)bxj.gpg
	echo "background (en/de)cryption finished `date +%T`" >> $(cloud)history
	-rm $(cloud)now-syncing/kalinin
	-rmdir $(cloud)now-syncing

