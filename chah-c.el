;;-----------------------------------------------------------------------
;;	Preferences for C and C++ code
;;-----------------------------------------------------------------------
;;;###autoload
(defun chah-c-style ()
  (setq comment-column 40)
  (make-local-variable 'indent-tabs-mode)
  (setq indent-tabs-mode t)			; Buffer-local
  (make-local-variable 'comment-line-start)
  (setq comment-line-start "/*--- ")
  (make-local-variable 'comment-line-start-skip)
  (setq comment-line-start-skip "/\\*-* *")
  ; Some syntactic elements are unknown to 19.34
  (if (assq 'innamespace c-offsets-alist)
      (c-set-offset 'innamespace '0))
  (if (assq 'inextern-lang c-offsets-alist)
      (c-set-offset 'inextern-lang '0))
  ;; Some projects have different requirements
  (let ((bfn (or (buffer-file-name) ""))) ; nil for some synthetic buffers
    (cond ((or (string-match "/micropython" bfn)
	       (string-match "/mbed" bfn)
	       (string-match "/mcurses" bfn)
	       (string-match "/incurses" bfn)
	       (string-match "/p-csl4" bfn)
	       (string-match "/ulc4" bfn)
	       (string-match "/kk1rt" bfn)
	       (string-match "/csdn" bfn)
	       (string-match "/p-vxpdf" bfn)
	       (string-match "/d-xsl8" bfn)
	       (string-match "/urtc" bfn)
	       (string-match "/xsl8" bfn))
	   (setq c-basic-offset 4)
	   (setq indent-tabs-mode nil))
	  ((or (string-match "/EPICS/" bfn))
	   (setq c-basic-offset 4)
	   (setq indent-tabs-mode nil)
	   (c-set-offset 'arglist-intro 8)
	   (c-set-offset 'arglist-cont-nonempty 8)
	   (c-set-offset 'arglist-cont 0))
	  ((or (string-match "/atto" bfn)
	       (string-match "/f2c/" bfn))
	   (setq c-basic-offset 8)
	   (setq indent-tabs-mode t))
	  ((or (string-match "/p-" bfn)
	       (string-match "/r-" bfn)
	       (string-match "/dtacq/LOWLATENCY/" bfn))
	   (setq c-basic-offset 2))
	  ((or (string-match "/das2.0/" bfn))
	   (setq indent-tabs-mode t)
	   (setq tab-width 4)
	   (setq c-basic-offset 4)
	   ;; Leave Crystal's tabbing in DAS_HMI & my old in Lib/App/Util
	   (when (or (string-match "/das2.0/DAS_Application/" bfn)
		     (string-match "/das2.0/HMI/" bfn))
	     (add-clang-format-save-hook)))
	  ((or (string-match "/business-logic/" bfn)
	       (string-match "/bl2/" bfn)
	       (string-match "/cpp-doodles/" bfn)
	       (string-match "/pmac-tests/" bfn))
	   (setq indent-tabs-mode t)
	   (setq tab-width 4)
	   (setq c-basic-offset 4))
	  ;; mastu project has many indentation styles
	  ;;((or (string-match "/mastu/" bfn))
	  ;; (setq indent-tabs-mode t)
	  ;; (setq tab-width 8)
	  ;; (setq c-basic-offset 8))
	  (t
	   (setq c-basic-offset 4)
	   (setq indent-tabs-mode nil)))
    (when (or (string-match "/micropython" bfn)
	      (string-match "/mbed" bfn))
      (setq comment-line-start "// ")
      (setq comment-line-start-skip "// *"))))

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
