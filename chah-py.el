;;-----------------------------------------------------------------------
;;	Preferences for Python code
;;-----------------------------------------------------------------------
;;;###autoload
(defun chah-py-style ()
  ;; Some projects have different requirements
  (let ((bfn (or (buffer-file-name) ""))) ; nil for some synthetic buffers
    (cond ((or (string-match "/dsm-pcs/codegen/" bfn))
	   (setq python-indent-offset 8)
	   (setq indent-tabs-mode t))
	  (t
	   (setq python-indent-offset 4)
	   (setq indent-tabs-mode nil)))))
