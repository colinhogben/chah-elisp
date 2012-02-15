;;=======================================================================
;;	chah's preferences
;;=======================================================================

;;;###autoload
(defun cpt ()
  (interactive)
  (modify-frame-parameters (selected-frame)
			   (list
			    (cons 'name (concat "Emacs: "
						(user-login-name)
						":cpt@"
						(system-name))))))
