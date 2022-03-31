AUTOFILE = ch-auto.el

# For 19.34, use update-autoloads-from-directory ?
# But even this fails with: Search failed: "^L"

update:	FORCE
	env AUTOFILE=$(AUTOFILE) emacs --batch -l updateauto.el

.PHONY:	FORCE
