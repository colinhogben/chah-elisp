;;; Pinched from mjn
;;; F601 chah	Added default routine

;;;###autoload
(defun find-routine-or-line (dest)
  "Move to the specified routine or line number."
  (interactive (find-routine-interactive "Routine or line: "))
  (if (string-match "^[0-9]+$" dest)
      (goto-line (string-to-int dest))
    (find-routine dest)))

;;;###autoload
(defun find-routine (name)
  "Find the specified routine name in the current buffer.
If found, the point will be placed following the routine name
and before any parameter declarations.
Requires that function `XYZ-find-routine' is bound when the major-mode
is XYZ-mode."
  (interactive (find-routine-interactive "Find routine: "))
  (let* ((symname (symbol-name major-mode))
	 (funcname (concat (substring symname 0 -5) "-find-routine"))
	 (funcsym (intern funcname)))
    (or (fboundp funcsym) (error "Function %s not defined" funcname))
    (funcall funcsym name)))

;; Read destination with a default
(defun find-routine-interactive (prompt)
  (list
   (let* ((default (find-routine-default))
	  (spec (read-string (if default
				 (format "%s(default %s) " prompt default)
			       prompt))))
     (if (equal spec "")
	 (or default (error "There is no default routine"))
       spec))))

;; Read a default routine name from point.  Copied from find-tag-default.
(defun find-routine-default ()
  (save-excursion
    (while (looking-at "\\sw\\|\\s_")
      (forward-char 1))
    (if (or (re-search-backward "\\sw\\|\\s_"
				(save-excursion (beginning-of-line) (point))
				t)
	    (re-search-forward "\\(\\sw\\|\\s_\\)+"
			       (save-excursion (end-of-line) (point))
			       t))
	(progn (goto-char (match-end 0))
	       (buffer-substring (point)
				 (progn (forward-sexp -1)
					(while (looking-at "\\s'")
					  (forward-char 1))
					(point))))
      nil)))

(defun fortran-find-routine (name)
  "Find the specified routine name in the current buffer.
If found, the point will be placed following the routine name
and before any parameter declarations."
  (interactive "sFind routine: ")
  (let* ((here (point)))
    (goto-char (point-min))
    (if (re-search-forward
	 (concat "\\(function\\|subroutine\\|entry\\|block *data\\) +"
		 name)
	 (point-max) t)
	(progn
	  (push-mark here)
	  (re-search-forward "[ ]*(" (save-excursion (end-of-line) (point)) t))
      (progn
	(goto-char here)
	(error "Routine %s not found" name)))))

(defun cpl-find-routine (name)
  "Find the specified routine name in the current buffer.
If found, the point will be placed following the routine name
and before any parameter declarations."

  (interactive "sFind routine: ")
  (let* ((here (point)))
    (goto-char (point-min))
    (if (re-search-forward
	 (concat "\\(function\\|subroutine\\|program\\) +" name)
	 (point-max) t)
	(push-mark here)
      (progn
	(goto-char here)
	(error "Routine %s not found" name)))))

(defun emacs-lisp-find-routine (name)
  "Find the specified routine name in the current buffer.
If found, the point will be placed following the routine name
and before any parameter declarations."

  (interactive "sFind routine: ")
  (let* ((here (point)))
    (goto-char (point-min))
    (if (re-search-forward
	 (concat "^(defun[ \t]+" name)
	 (point-max) t)
	(push-mark here)
      (progn
	(goto-char here)
	(error "Routine %s not found" name)))))

(defun c-find-routine (name)
  "Find the specified routine name in the current buffer.
If found, the point will be placed following the routine name
and before any parameter declarations."
  (interactive "sFind routine: ")
  (let* ((here (point))
	 (case-fold-search t)
	 found)
    (goto-char (point-min))
    (while (and (not found)
		(re-search-forward
		 (concat "^\\([a-z0-9_][a-z0-9_ ]*[ \t]\\**\\)?"
			 name "[a-z0-9_]*[ \t]?(")
		 (point-max) t))
      (save-excursion
	(backward-char)
	(forward-sexp)
	(skip-chars-forward " \t\n")
	(if (looking-at ";")
	    nil
	  (push-mark here)
	  (setq found t))))
    (if (not found)
	(progn
	  (goto-char here)
	  (error "Routine %s not found" name)))))

(defun perl-find-routine (name)
  "Find the specified routine name in the current buffer.
If found, the point will be placed following the routine name."
  (interactive "sFind routine: ")
  (let* ((here (point))
	 (case-fold-search nil))
    (goto-char (point-min))
    (if (re-search-forward
	 (concat "^sub[ \t]+" name)
	 (point-max) t)
	(push-mark here)
      (progn
	(goto-char here)
	(error "Routine %s not found" name)))))

(defun tcl-find-routine (name)
  "Find the specified routine name in the current buffer.
If found, the point will be placed following the routine name."
  (interactive "sFind routine: ")
  (let* ((here (point))
	 (case-fold-search nil))
    (goto-char (point-min))
    (if (re-search-forward
	 (concat "\\bproc[ \t]+{?" (regexp-quote name))
	 (point-max) t)
	(push-mark here)
      (progn
	(goto-char here)
	(error "Routine %s not found" name)))))

(defun sh-find-routine (name)
  "Find the specified routine name in the current buffer.
If found, the point will be placed following the routine name."
  (interactive "sFind routine: ")
  (let* ((here (point))
	 (case-fold-search nil))
    (goto-char (point-min))
    (if (re-search-forward
	 (concat "^" (regexp-quote name) "\\w*()")
	 (point-max) t)
	(progn
	  (skip-chars-backward "()")
	  (push-mark here))
      (progn
	(goto-char here)
	(error "Routine %s not found" name)))))

(defun python-find-routine (name)
  "Find the specified function or class name in the current buffer.
If found, the point will be placed following the routine name."
  (interactive "sFind routine: ")
  (let* ((here (point))
	 (case-fold-search nil))
    (goto-char (point-min))
    (if (re-search-forward
	 (concat "^[ \t]*\\(def\\|class\\)[ \t]+" (regexp-quote name))
	 (point-max) t)
	(push-mark here)
      (progn
	(goto-char here)
	(error "Routine %s not found" name)))))
