;;=======================================================================
;;	Functions and key bindings for chah
;;=======================================================================

(defvar chah-location 'unknown
  "Token indicating where we are, for later location-specific customisation.")

(defun chah ()
  "Pull in chah definitions."
  (interactive)

  ;; Load the autoload file, if it exists
  (load "ch-auto" t)

  ;; Other preferences
  (setq require-final-newline 'ask)	;; Check when writing with no final \n
  (setq dired-dwim-target t)		;; Guess other buffer when copying

  ;; Colin's extra bindings
  ;; Make F12 behave like C-x - most important ergonomic improvement
  (global-set-key [f12] 'Control-X-prefix)

  (global-set-key [f2] 'find-routine-or-line)
  (global-set-key [f3] 're-search-forward-again)
  (global-set-key [(shift f3)] 're-search-forward-new)
  (global-set-key [f4] 'next-error)
  ;; 2004-10-18 Shift with function keys not working via Citrix+ssh
  (global-set-key [(shift f4)] 'compile)
  (global-set-key [f5] 'set-mark-command)
  (global-set-key [(shift f5)] 'delete-indentation)
  (global-set-key [f6] 'downcase-word)
  (global-set-key [(shift f6)] 'upcase-word)
  (global-set-key [f7] 'search-forward-again)
  (global-set-key [(shift f7)] 'search-forward-new)
  (global-set-key [f8] 'comment-block)
  (global-set-key [(shift f8)] 'font-lock-mode)
  (global-set-key [f9] 'advertised-undo)
  (global-set-key [f10] 'kill-whole-line)
  (global-set-key [(shift f11)] 'manual-entry)
  (global-set-key [find] 'beginning-of-buffer) ;Home
  (global-set-key [select] 'end-of-buffer) ;End
  (global-set-key [delete] 'backward-delete-char-untabify)
  (global-set-key [kp-1] 'backward-word)
  (global-set-key [kp-end] 'backward-word)
  (global-set-key [kp-3] 'forward-word)
  (global-set-key [kp-next] 'forward-word)
  (global-set-key [(meta escape) \[ \5 \~] 'scroll-other-window-down) ;Alt-PgUp
  (global-set-key [(meta escape) \[ \6 \~] 'scroll-other-window) ;Alt-PgDn
  (global-set-key [(meta prior)] 'scroll-other-window-down) ;Alt-PgUp
  (global-set-key [(meta next)] 'scroll-other-window) ;Alt-PgDn

  ;; OVDC -> chooser key weirdness
  (global-set-key [f33] 'backward-word)	; KP-1
  (global-set-key [f35] 'forward-word)	; KP-3
  (global-set-key [SunF37] 'Control-X-prefix) ; F12

  (define-key esc-map "?" 'what-line)
  (define-key esc-map ":" 'copy-comment-to-other-window)
  (define-key esc-map [return] 'back-to-indentation)
  (define-key esc-map "\C-M" 'back-to-indentation)

  (define-key ctl-x-map "/" 'point-to-register)
  (define-key ctl-x-map "j" 'jump-to-register)
  (define-key ctl-x-map "F" 'find-file-at-point)
  (define-key ctl-x-map "W" 'write-region)
  (define-key ctl-x-map "_" 'bury-buffer)
  (define-key ctl-x-map "%" 'query-replace-regexp)
  (define-key ctl-x-map [return] 'find-shell)
  (define-key ctl-x-map "\C-M" 'find-shell)
  (define-key ctl-x-map "I" 'insert-source)

  (eval-after-load "vc" '(define-key vc-prefix-map "$" 'ediff-revision))

  (condition-case err
      (progn
	(server-start)
	(setenv "EDITOR" "emacsclient"))
    (error (message "Starting server: %s" (error-message-string err))))

  (when (memq chah-location '(mythic))
    (add-to-list 'auto-mode-alist '("\\.t$" . perl-mode)) ; Perl tests
    (load "curl-style" nil t)
    (load "my-style" nil t))

  (when (memq chah-location '(jac laptop win)) ; or based on version?
    ;; See SO 3072648
    (require 'ansi-color)
    (defun colourise-compilation-buffer ()
      (let ((inhibit-read-only t))
	(ansi-color-apply-on-region (point-min) (point-max))))
    (add-hook 'compilation-filter-hook 'colourise-compilation-buffer))

  (when (memq chah-location '(jac linux laptop))
    (if (fboundp 'conf-mode)
	(add-to-list 'auto-mode-alist
		     ;; systemd files
		     '("\\.\\(service\\|path\||mount\\)$" . conf-mode)))
    (if (fboundp 'c-mode)
	(add-to-list 'auto-mode-alist
		     ;; arduino sketches
		     '("\\.ino$" . c-mode))))

  ;; In absence of autoload machinery, load stuff directly
  (when (memq chah-location '(pepperpot mythic dinsdale))
    ;; RCS support with C-x v... bindings
    (load "vc" nil t)
    ;; Shortcuts to FTP sites
    (load "chah-ftp" nil t))
)
