(defvar insert-source-directory nil
  "Directory containing files used by \\[insert-source]")
;; N.B. source-directory indicates where emacs was built.
;; Inadvertently used by thos module - will attempt to phase it out.
(defvar source-directory nil
  "Directory containing files used by \\[insert-source]")

;;;###autoload
(defun insert-source (filename)
  "Insert source file FILENAME from source-directory."
  (interactive (list (read-file-name "File: "
				     (or insert-source-directory
					 source-directory ; deprecated
					 default-directory)
				     nil
				     t)))
  (insert-file-contents (expand-file-name filename
					  (or source-directory
					      default-directory))))

