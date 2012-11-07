;; Make a new shell buffer with a name optionally supplied by the user.
;; Allows you easily to have more than one shell.
;; chah@jet.uk  11 May 1993

(require 'shell)

(defun new-shell (&optional bufname)
  "Run an inferior shell, with I/O through buffer BUFNAME.
Like shell (q.v.), but you supply the buffer name."
  (interactive (list (read-string (format "Buffer name: (default %s) "
					  (new-shell-buffer-name)))))
  (if (or (null bufname) (string= bufname ""))
      (setq bufname (new-shell-buffer-name))
    (or (string-match "^\\*.*\\*$" bufname)
	(error "Buffer name must begin and end with *"))
    (and (get-buffer bufname)
	 (error "Buffer name \"%s\" is in use" bufname)))
  (cond ((not (comint-check-proc bufname))
	 (let* ((prog (or explicit-shell-file-name
			  (getenv "ESHELL")
			  (getenv "SHELL")
			  "/bin/sh"))		     
		(name (file-name-nondirectory prog))
		(startfile (concat "~/.emacs_" name))
		(xargs-name (intern-soft (concat "explicit-" name "-args"))))
	   (set-buffer (apply 'make-comint (substring bufname 1 -1) prog
			      (if (file-exists-p startfile) startfile)
			      (if (and xargs-name (boundp xargs-name))
				  (symbol-value xargs-name)
				  '("-i"))))
	   (shell-mode))))
  (switch-to-buffer bufname))

(defun new-shell-buffer-name ()
  "Return the first unused name in the sequence *shell* *shell2* *shell3* ..."
  (let ((bufname "*shell*")
	(num 1))
    (while (get-buffer bufname)
      (setq bufname (format "*shell%d*" 
			    (setq num (1+ num)))))
    bufname))
