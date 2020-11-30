
(defun detect-header (level)
  (save-excursion
  (let ((stars (concat (apply #'concat (cl-loop repeat level collect "\\*")) " ")))
    (when-let ((header (search-forward-regexp (concat "^" stars) nil t)))
              (beginning-of-line) (point)))))

(defun moveto-header (level)
  (when-let ((DH (detect-header level)))
    (goto-char DH)))

(defun section-counter(level)
  (if (when (moveto-header level) (org-show-subtree) (end-of-line) t)
      (1+ (section-counter level)) 0))

(defun derive-version(change-log.org)
(find-file change-log.org)
(let ((r (list
(progn
  (goto-char (point-min))
  (let ((r(section-counter 1)))
    (if(= 0 r) r (1- r))))
(progn
  (goto-char (point-min))
  (when (moveto-header 1) (org-show-subtree) (end-of-line))
  (section-counter 2))
(progn
  (goto-char (point-min))
  (when (moveto-header 1)(org-show-subtree) (end-of-line))
  (when (moveto-header 2)(org-show-subtree) (end-of-line))
  (section-counter 3)))))
(kill-buffer) r))

(defun format-version(change-log.org)
  (let ((r (derive-version change-log.org)))
    (format "%d.%d.%d" (first r) (second r) (third r))))

(defun printangle(FN)
  "to be used in Makefile instead of org-babel-tangle-file"
  (let ((l (length default-directory)))
    (apply #'concat (mapcar #'(lambda(x) (substring (format "%s " x) l)) (org-babel-tangle-file FN)))))
