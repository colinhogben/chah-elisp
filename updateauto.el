(let ((generated-autoload-file
       (expand-file-name (getenv "AUTOFILE"))))
  (batch-update-autoloads))
