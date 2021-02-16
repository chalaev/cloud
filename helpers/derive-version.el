;; part of lisp-goodies package
;; https://github.com/chalaev/lisp-goodies
;; generates version number based on the structure of the Change Log file

(defun detect-header (level)
  (save-excursion
  (let ((stars (concat (apply #'concat (cl-loop repeat level collect "\\*")) " ")))
    (when-let ((header (search-forward-regexp (concat "^" stars) nil t)))
              (beginning-of-line) (point)))))

(defun moveto-header (level &optional max-point)
  (when-let ((DH (detect-header level)))
    (when (< DH (or max-point (point-max)))
      (goto-char DH))))

(defun section-counter(level &optional max-point)
  (when (moveto-header level max-point)
    (org-show-subtree)
    (end-of-line)
    (cons
     (point)
     (section-counter level max-point))))

(defun m1(N) (if(= 0 N) N (1- N)))
  
(defun derive-version(change-log.org)
(find-file change-log.org)
(let(MC)
(list
(progn
  (goto-char (point-min))
  (let((SC (section-counter 1)))
    (when (cadr SC) (setf MC (cadr SC)))
    (m1(length SC))))
(progn
  (goto-char (point-min))
  (when (moveto-header 1) (org-show-subtree) (end-of-line))
  (let((SC (section-counter 2 MC)))
    (when (cadr SC) (setf MC (cadr SC)))
    (m1(length SC))))
(prog1
    (progn
      (message "3 MC=%d" MC)
      (goto-char (point-min))
      (let((P(point)))
	(ifn (and(moveto-header 1) (< (point) MC))
	     (goto-char P)
	     (org-show-subtree) (end-of-line)))
      (let((P(point)))
	(ifn (and(moveto-header 2) (< (point) MC))
	     (goto-char P)
	     (org-show-subtree) (end-of-line)))
  (m1(length(section-counter 3 MC))))
  (kill-buffer)))))

(defun format-version(change-log.org)
  (let ((r (derive-version change-log.org)))
    (format "%d.%d.%d" (first r) (second r) (third r))))

(defun printangle(FN)
  "to be used in Makefile instead of org-babel-tangle-file"
  (let ((l (length default-directory)))
    (apply #'concat (mapcar #'(lambda(x) (substring (format "%s " x) l)) (org-babel-tangle-file FN)))))
