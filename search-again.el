;; Non-incremental searches

(defvar search-last-string nil)
(defvar search-last-regexp nil)

;; Get interactive args for {re-}search-{forward,backward}
(defun search-arg (prompt last)
  (if (or current-prefix-arg (null last))
      (list (read-string prompt))
    nil))

;;;###autoload
(defun search-forward-again (&optional string)
  "Search forward for STRING, or repeat last search if nil.
Called interactively, prompts for a new string if prefix arg given."
  (interactive (search-arg "Search: " search-last-string))
  (search-forward-new (or string search-last-string)))

;;;###autoload
(defun search-forward-new (string)
  "Search forward for STRING."
  (interactive "sSearch: ")
  (if (> (length string) 0)
      (progn
	(setq search-last-string string)
	(or (search-forward string nil t)
	    (error "Search failed: \"%s\"" string)))))

;;;###autoload
(defun search-backward-again (&optional string)
  "Search backward for STRING, or repeat last search if nil.
Called interactively, prompts for a new string if prefix arg given."
  (interactive (search-arg "Search: " search-last-string))
  (search-backward-new (or string search-last-string)))

;;;###autoload
(defun search-backward-new (string)
  "Search backward for STRING."
  (interactive "sSearch: ")
  (if (> (length string) 0)
      (progn
	(setq search-last-string string)
	(or (search-backward string nil t)
	    (error "Search failed: \"%s\"" string)))))

;;;###autoload
(defun re-search-forward-again (&optional regexp)
  "Search forward for REGEXP, or repeat last search if nil.
Called interactively, prompts for a new regexp if prefix arg given."
  (interactive (search-arg "RE search: " search-last-regexp))
  (re-search-forward-new (or regexp search-last-regexp)))

;;;###autoload
(defun re-search-forward-new (regexp)
  "Search forward for REGEXP."
  (interactive "sRE search: ")
  (if (> (length regexp) 0)
      (progn
	(setq search-last-regexp regexp)
	(or (re-search-forward regexp nil t)
	    (error "Search failed: \"%s\"" regexp)))))
