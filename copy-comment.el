;;=======================================================================
;;	Copy comments between windows
;;=======================================================================

;;;###autoload
(defun copy-comment-to-other-window ()
  "Copy a comment from one window to another, adjusting for major mode."
  (interactive)
  (let ((comment (or (grab-comment)
		     (error "No comment on this line"))))
    (other-window 1)
    (if (eq major-mode 'c-mode)
	(c-indent-for-comment)
      (indent-for-comment))
    (insert comment)
    (end-of-line)))

(defun grab-comment ()
  "Extract the text of a comment."
  (save-excursion
    (let ((eol (save-excursion (end-of-line) (point)))
	  boc eoc)
      (beginning-of-line)
      (if (not (or (and comment-line-start-skip
			(re-search-forward comment-line-start-skip eol t))
		   (and comment-start-skip
			(re-search-forward comment-start-skip eol t))))
	  nil
	(setq boc (or (match-end 1) (match-end 0)))
	(and (> (length comment-end) 0)
	     (search-forward comment-end eol t)
	     (setq eoc (- (point) (length comment-end))))
	(buffer-substring boc (or eoc eol))))))
