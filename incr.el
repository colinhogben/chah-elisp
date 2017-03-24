;;-----------------------------------------------------------------------
;;	Increment number
;;-----------------------------------------------------------------------
;;;###autoload
(defun increment-region (start end &optional increment)
  (interactive "r\nP")
  (let* ((old (buffer-substring start end))
	 (iold (string-to-int old))
	 (iinc (if (null increment) 1 increment))
	 (inew (+ iold iinc))
	 (new (int-to-string inew)))
    (message "old[%s] new[%s] incr[%s]" old new increment)
    (delete-region start end)
    (goto-char start)
    (insert new)))
