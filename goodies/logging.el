;;; -*- mode: Emacs-Lisp;  lexical-binding: t; -*-

;; generated from https://github.com/chalaev/elisp-goodies/blob/master/goodies.org
(unless (boundp '*log-level*) (defvar *log-level* 0))
(unless (boundp '*emacs-d*) (defvar *emacs-d* (concat (getenv "HOME") "/.emacs.d/")))

(unless (boundp '*file-acc-buffer*) (defvar *file-acc-buffer* nil))
(defvar *last-FLD* nil "saves last day printed to the log file")

(defun clog-flush()
  "save log messages to file for debugging"
  (when (= 0 *log-level*)
    (with-temp-buffer
      (let ((today-str (format-time-string "%04Y-%02m-%02d" (current-time))))
        (unless (string= today-str *last-FLD*)
          (setf *last-FLD* today-str)
          (insert today-str) (newline))
        (dolist (msg (reverse *file-acc-buffer*))
          (insert msg) (newline)))
      (append-to-file (point-min) (point-max) (concat *emacs-d* "elisp.log")))
    (setf *file-acc-buffer* nil)))

(defun file-acc-push(msg)
  (push msg *file-acc-buffer*)
  (when (< 30 (length *file-acc-buffer*)) (clog-flush)))

(defun clog (level fstr &rest args)
  "simple logging function" ; level is one of → :debug :info :warning :error
  (when (<= *log-level* (or (pos level '(:debug :info :warning :error)) 0))
    (let ((log-msg
           (cons
            (concat "%s " (format-time-string "%H:%M:%S "
(apply 'encode-time (butlast (decode-time (current-time)) 3)))
                    fstr)
            (cons (symbol-name level) args))))
      (file-acc-push (apply #'format log-msg))
      (apply #'message log-msg))))

(defun on-emacs-exit()
  (clog :debug "flushing comments before quiting emacs")
  (clog-flush))

(add-hook 'kill-emacs-hook 'on-emacs-exit)
