(let ((autofile (or (getenv "AUTOFILE") "ch-auto.el")))
  (cond
   ((fboundp 'loaddefs-generate)
    (loaddefs-generate "." autofile))
   ((fboundp 'make-directory-autoloads)
    (make-directory-autoloads "." autofile))
   ((fboundp 'update-directory-autoloads)
    (let ((generated-autoload-file (expand-file-name autofile)))
      (update-directory-autoloads ".")))
   (t
    (let ((generated-autoload-file (expand-file-name autofile)))
      (batch-update-autoloads)))))
