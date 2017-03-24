(defvar source-directory nil
  "Directory containing files used by \\[insert-source]")

;;;###autoload
(defun insert-source (filename)
  "Insert source file FILENAME from source-directory."
  (interactive (list (read-file-name "File: "
				     (or source-directory
					 default-directory)
				     nil
				     t)))
  (insert-file-contents (expand-file-name filename
					  (or source-directory
					      default-directory))))

