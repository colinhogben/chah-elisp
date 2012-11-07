;;-----------------------------------------------------------------------
;;	Shell buffer
;;-----------------------------------------------------------------------

;;;###autoload
(defun find-shell ()
  "Switch to the *shell* buffer or start a new one.
This is better than the `shell' command since it does not lose the marks
if the buffer already exists."
  (interactive)
  (let ((buffer (get-buffer "*shell*")))	; do we have one already?
    (if buffer
	(switch-to-buffer buffer)		; yes - go to it
      (shell))))				; no - start a new one
