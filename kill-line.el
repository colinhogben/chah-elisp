;; kill-whole-line
;; Delete the whole of the current line, regardless of whether or not it
;; contains text, and including the terminating newline.
;;

;;;###autoload
(defun kill-whole-line (&optional arg)
  "Kill the whole of the current line.
With prefix argument, kill that many lines from point."
  (interactive "*P")

  (beginning-of-line)
  (kill-region
   (point)
   (progn
     (cond
       (arg (forward-line (prefix-numeric-value arg)))
       ((eobp) (signal 'end-of-buffer nil))
       (t (forward-line 1)))
     (point))))

;; kill-to-char
;; Implements a Sintran-like C-d command
;;
;;;###autoload
(defun kill-to-char (c)
  "Kill up to and including the first occurence of CHAR in the current line.
If CHAR is CR or LF, kill up to but not including the end of the line.
If CHAR is C-r, kill back to the beginning of the line.
If CHAR is C-d, kill the entire current line."
  (interactive "cKill to char:")

  (let ((here (point))
	(bol (save-excursion (beginning-of-line) (point)))
	(eol (save-excursion (end-of-line) (point))))
    (cond
     ((or (eq c ?\n) (eq c ?\r))
      (delete-char (- eol (point)) t))
     ((eq c ?\C-r)
      (delete-char (- bol (point)) t))
     ((eq c ?\C-d)
      (kill-whole-line))
     ((search-forward (char-to-string c) eol t)
      (delete-char (- here (point)) t))
     (t (error "Not found")))))
