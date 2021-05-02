;; -*-  lexical-binding: t; -*-
(defun get-file-properties* (FN)
(when FN
  (or (cloud-locate-FN FN) (cloud-get-file-properties(file-chase-links FN)))))

(defun cip-ext (FN)
"extension of encrypted file based on the original name"
(case* (file-name-extension FN) string=
       ("jpeg" ".png")
       ("jpg" ".png")
       ("png" ".png")
       (otherwise ".gpg")))

(defun forget-password(XYZ)
  "removes image password from password file"
(let* ((str (progn
	     (find-file (image-passes))
	     (buffer-string)))
       (BN (buffer-name)))
  (with-temp-file (image-passes)
    (insert (replace-regexp-in-string (format "%s .*
" XYZ) "" str)))
  (kill-buffer BN)))

(defmacro bad-column (cType N &optional str)
(if str
`(clog :error "invalid %dth column in %s line = %s" ,N ,cType ,str)
`(clog :error "invalid %dth column in %s line" ,N ,cType)))

(defun gpg-encrypt(FN XYZ)
(= 0 (shell-command
  (format "gpg --batch --yes --pinentry-mode loopback --passphrase %S -o %s --symmetric %s" password (FN remote-directory (concat XYZ ".gpg")) (untilde FN)))))

(defun gpg-decrypt(FN XYZ)
(= 0 (shell-command 
(format "gpg --batch --yes --pinentry-mode loopback --passphrase %S -o %s --decrypt %s" password (untilde FN) (FN remote-directory (concat XYZ ".gpg"))))))

(defun replace-file-ext(FN new-ext)
  "replacing file extension"
  (concat (file-name-sans-extension FN) "." new-ext))

(defun youngest(&rest FNs)
  (car (sort FNs #'file-newer-than-file-p)))
