AUTOFILE = ch-auto.el

$(AUTOFILE):	$(filter-out $(AUTOFILE),$(wildcard *.el))
	emacs --batch --eval '(let ((generated-autoload-file (expand-file-name "$@"))) (update-directory-autoloads "."))'
