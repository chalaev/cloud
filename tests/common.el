;; -*-  lexical-binding: t; -*-
;; common.el – *first* file to be loaded
;; (require 'shalaev)

(require 'el-debug); debug-make-dir must be defined in Makefile
(load(FN debug-make-dir "tests" "ert.el")); debug-make-dir must be defined in Makefile

(if(and(boundp 'debug-make-dir) debug-make-dir)
  (setf debug-make-dir(file-name-as-directory debug-make-dir))
  (clog :error "debug-make-dir is either undefined or nil"))

(defvar debug-main-conf (read-conf-file (FN debug-make-dir "tests" "common.conf")) "prepared in tests/prepare.el")
(defvar root-test-dir nil "temporary directory for all virtual hosts")
(debug-set debug-main-conf root-test-dir)

(defmacro debug-environment(&rest body)
"sets most common variables"
`(letc debug-main-conf; global test configuration (common for all hosts)
(remote-directory remote/files password root-test-dir ((:string) hostnames))
(clean-RD remote-directory)
,@body))

(defvar debug-host-confs
  (mapcar #'(lambda(HN)(cons HN (read-conf-file (FN root-test-dir (concat HN ".conf")))))
    (letc debug-main-conf (((:string) hostnames)) hostnames)))

(defvar debug-host-conf nil "this global definition needed for loaded main file"); 
(defmacro host>(HN &rest body); HN = "host name"
"to be called inside debug-environment"
`(ifn-let((debug-host-conf(cdr(assoc ,HN debug-host-confs))))
(clog :error "host %s is unconfigured" ,HN)
(load(FN debug-make-dir "tests" "cloud.el")); ← loading main file
(letc debug-host-conf (localhost ~ emacs-d *config-directory* local-dir local/host/conf dot-dir dir-1 dir-1a dot-file conf-file tmp-file file-1 file-1a file-2)
,@body
(clog :debug "leaving %s with these files in %s:
%s" ,HN remote-directory (together(directory-files remote-directory nil))))))

(defmacro debug-set*(&rest var-names)
`(debug-set (append debug-main-conf debug-host-conf) ,@var-names))

(defmacro clean-RD(dir)
"cleans remote directory by deleting and re-creating it"
`(ifn(string-prefix-p "/tmp/" ,dir) (clog :error "test cloud-init> remote-directory= %s must be inside tmp!")
(delete-directory ,dir t)
(need-dir ,dir)))
