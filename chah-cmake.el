;;=======================================================================
;;	Preferences for CMake
;;=======================================================================
;;;###autoload
(defun chah-cmake-style ()
  (let ((bfn (or (buffer-file-name) ""))) ; nil for some synthetic buffers
    (cond ((or (string-match "/business-logic/" bfn))
	   (setq cmake-tab-width 8)))))

