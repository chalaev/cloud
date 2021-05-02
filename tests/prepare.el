;; prepare.el
(require 'shalaev)
(defvar root-test-dir (file-name-as-directory(make-temp-file "cloud.test." t)) "root directory for all tests")

(let(
(hostnames '("hostA" "hostB")); values of the localhost variable
(remote-directory (need-dir root-test-dir "mnt/remote")); remote directory is mounted differently on different hosts
(remote/files "AbC")
(password "12345"))
(write-conf(untilde (FN debug-make-dir "tests" "common.conf"))
 (make-conf root-test-dir hostnames remote-directory remote/files password))

(dolist(localhost hostnames)
(let*((signature localhost); [was (rand-str 3)] distinguishes files having the same name, but residing on different hosts
(s(format "
%s
" signature))
(configured "yes")
(~ (need-dir root-test-dir localhost))
(emacs-d (tilde(need-dir ~ ".emacs.d")))
(*config-directory* (tilde(need-dir emacs-d "conf"))); overwrites the definition in ~/.emacs.d/local-packages/shalaev.el
(local-dir (need-dir *config-directory* "cloud"))
(local/host/conf (concat local-dir localhost ".conf"))
(dot-dir (tilde(need-dir ~ (concat ".config-dir." signature)))); an example of hidden directory – all files inside =dot-dir= are blacklisted
(dir-1   (tilde(need-dir ~ (concat "dir-1." signature))))
(dir-1a  (tilde(need-dir ~ "dir-1a")))
(dot-file  (tilde(echo-to-file (concat "~/.bash-config." signature) (concat "(blacklisted) dot-file" s))))
(conf-file (tilde(echo-to-file (concat dot-dir "file." signature) "any non-whitelisted file inside blacklisted directory is blacklisted")))
(tmp-file  (tilde(echo-to-file (concat "~/tmp-1." signature) "blacklisted")))
(file-1    (tilde(echo-to-file (concat "~/file-1." signature) (concat "an ordinary file (missing on another host)" s))))
(file-1a   (tilde(echo-to-file "~/file-1a.dat" (concat "an ordinary file; there is a file with the same name (but different content) on another host)" s))))
(file-2 (tilde(echo-to-file (concat  "~/tmp-2." signature) (concat "blacklisted " signature)))); blacklisted
(black-root-dirs(mapcar #'(lambda(DN)(concat root-test-dir signature DN)) (split-string "/mnt/ /tmp/ /etc/ /ssh:")))
(black-matches (split-string "tmp /old /log /Downloads /.git/")); the same for both hosts
(black-extensions (split-string "aux idx ilg ind bak bbl blg brf bst dvi log out ps wav")))

(write-conf (untilde (FN root-test-dir (concat localhost ".conf")))
  (make-conf localhost ~ emacs-d *config-directory* local-dir local/host/conf dot-dir dir-1 dir-1a dot-file conf-file tmp-file file-1 file-1a file-2))

(write-conf (untilde local/host/conf)
  (make-conf configured remote-directory remote/files password)))))
