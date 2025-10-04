;;-----------------------------------------------------------------------
;;	Preferences for C and C++ code
;;-----------------------------------------------------------------------
(c-add-style "chah-base"
	     '((c-basic-offset . 4)
	       (indent-tabs-mode . nil)
	       (comment-column . 40)
	       (comment-line-start . "/*--- ")
	       (comment-line-start-skip . "/\\*-* *")
	       (c-offsets-alist
		. (
		   ;; C++
		   (innamespace . 0)
		   (inextern-lang . 0)	; extern "C" {
		   (inline-open . 0)	; inline method open brace
		   ))
	       ))
(c-add-style "chah-pcs"
	     '("chah-base"
	       (c-basic-offset . 3)
	       ))

;;;###autoload
(defun chah-c-style ()
  (let ((bfn (or (buffer-file-name) ""))) ; nil for some synthetic buffers
    (cond (;; Submodules of mastu
	   (or (string-match "/mpu/" bfn)
	       (string-match "/dtacq-ioproc/" bfn))
	   (c-set-style "chah-base"))
	  ((or (string-match "/mastu/" bfn)
	       (string-match "/nullpcs/" bfn))
	   (c-set-style "chah-pcs"))
	  (t
	   (c-set-style "chah-base")))))

(or (fboundp 'comment-dwim)		; In newer emacsen
    (define-key c-mode-map "\M-;" 'c-indent-for-comment))
(define-key c-mode-map "" 'newline-and-indent)

;; From emacs.stackexchange.com 48500
(defun add-clang-format-save-hook ()
  "Set a hook to run clang-format on write"
  (when (find-clang-format '("clang-format.el"
			     "clang-format/clang-format.el"
			     "clang-format-10/clang-format.el"))
    (add-hook 'before-save-hook
	      (lambda ()
		(when (locate-dominating-file "." ".clang-format")
		  (clang-format-buffer))
		;; Continue saving
		nil)
	      nil
	      ;; Buffer local hook
	      t)))

(defun find-clang-format (paths)
  (if (null paths)
      nil
    (condition-case err
	(progn
	  (require 'clang-format (car paths))
	  t)
      (error (find-clang-format (cdr paths))))))

(defun c-indent-for-comment ()
  "Indent this line's comment or insert an empty comment.
If there is nothing currently on the line, indent as for a command."
  (interactive "*")
  (beginning-of-line 1)
  (if (save-excursion (skip-chars-forward " \t") (not (eolp)))
      (indent-for-comment)
    (c-indent-line)
    (insert (or comment-line-start comment-start
		(error "No comment syntax defined")))
    (save-excursion (insert comment-end))))

(defun text-to-c (start end)
  "Convert text in region to C printf statements."
  (interactive "r")
  (save-restriction
    (narrow-to-region start end)
    (goto-char (point-min))
    (while (search-forward "\"" nil t)		; Quote quotes
      (replace-match "\\\"" nil t))
    (goto-char (point-min))
    (insert "printf(\"")
    (end-of-line)
    (insert "\\n\"")
    (forward-line 1)
    (while (< (point) (point-max))
      (insert "       \"")
      (end-of-line)
      (insert "\\n\"")
      (forward-line 1))
    (backward-char)
    (insert ");")))

(defun c-to-text (start end)
  "Convert printf statements produced by text-to-c back text."
  (interactive "r")
  (save-restriction
    (narrow-to-region start end)
    (goto-char start)
    (while (re-search-forward "^  [^\"]*\"" nil t)
      (replace-match "" nil t))
    (goto-char start)
    (while (re-search-forward "\\\\n\"\\();\\)?" nil t)
      (replace-match "" nil t))
    (goto-char start)
    (while (search-forward "\\\"" nil t)
      (replace-match "\"" nil t))))
