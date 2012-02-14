;;
;; make a comment block, ornamented according to major mode
;;

;;;###autoload
(defvar comment-block-tokens nil
  "Tokens for comment-block.")
(make-variable-buffer-local 'comment-block-tokens)

;;;###autoload
(defvar comment-block-tokens-alist
  '((c-mode "/*" "*/" " *")
    (fortran-mode "C")
    (cpl-mode ";")
    (emacs-lisp-mode ";;")
    (tex-mode "%")
    (latex-mode "%")
    (c++-mode "//")
    (asm-mode "*")
    (mimic-mode "com ")
    (sgml-mode "<!--*" "-->" "    *"))
  "Alist of modes and the start, end and middle tokens to use")

;;;###autoload
(defun comment-block (&optional thick)
  "Insert a comment block for use in routine headers, etc.
If THICK is non-nil then use === rather than ---."
  (interactive "*P")
  (apply 'make-comment-block
	 thick
	 (or comment-block-tokens
             (cdr (assoc major-mode
                         comment-block-tokens-alist)) ; Search the alist
	     '("#"))))				; Default token

(defun make-comment-block (thick start &optional end &optional mid)
  (let ((ch (if thick ?= ?-)))
    (beginning-of-line)
    (insert "\n\n\n")
    (backward-char 3)
    (insert start)
    (insert-char ch 71)
    (forward-line)
    (insert (or mid start))
    (tab-to-tab-stop)
    (forward-line)
    (insert (or mid start))
    (insert-char ch 71)
    (insert (or end ""))
    (forward-line -1)
    (end-of-line)))
