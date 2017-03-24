;;-----------------------------------------------------------------------
;;	Increment number
;;-----------------------------------------------------------------------
;;;###autoload
(defun increment-region (start end)
  (interactive "r")
  (let* ((old (buffer-substring start end))
	 (iold (string-to-int old))
	 (inew (1+ iold))
	 (new (int-to-string inew)))
    (message "old[%s] new[%s]" old new)
    (delete-region start end)
    (goto-char start)
    (insert new)))
