AUTOFILE = ch-auto.el

# For 19.34, use update-autoloads-from-directory
# But even this fails with: Search failed: "^L"

$(AUTOFILE):	$(filter-out $(AUTOFILE),$(wildcard *.el))
	emacs --batch --eval '(let ((generated-autoload-file (expand-file-name "$@"))) (update-directory-autoloads "."))'
