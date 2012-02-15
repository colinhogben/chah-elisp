;;=======================================================================
;;	Functions and key bindings for chah
;;=======================================================================

(defvar chah-location 'unknown
  "Token indicating where we are, for later location-specific customisation.")

(defun chah ()
  "Pull in chah definitions."
  (interactive)

  ;; Should generate autoloads for elisp ?
  (autoload (quote comment-block) "comment-block" "\
Insert a comment block for use in routine headers, etc.
If THICK is non-nil then use === rather than ---." t nil)
  (autoload (quote kill-whole-line) "kill-line" "\
Kill the whole of the current line.
With prefix agrument, kill that many lined from point." t nil)
  (autoload (quote find-routine-or-line) "findrout" "\
Move to the specified routine or line number." t nil)

  ;; Other preferences
  (setq require-final-newline 'ask)	;; Check when writing with no final \n
  (setq dired-dwim-target t)		;; Guess other buffer when copying

  ;; Colin's extra bindings
  ;; Make F12 behave like C-x - most important ergonomic improvement
  (global-set-key [f12] 'Control-X-prefix)

  (global-set-key [f2] 'find-routine-or-line)
  (global-set-key [f4] 'next-error)
  ;; 2004-10-18 Shift with function keys not working via Citrix+ssh
  (global-set-key [(shift f4)] 'compile)
  (global-set-key [f5] 'set-mark-command)
  (global-set-key [f6] 'downcase-word)
  (global-set-key [(shift f6)] 'upcase-word)
  (global-set-key [f8] 'comment-block)
  (global-set-key [(shift f8)] 'font-lock-mode)
  (global-set-key [f9] 'advertised-undo)
  (global-set-key [f10] 'kill-whole-line)
  (global-set-key [f12 ?_] 'bury-buffer)
  (global-set-key [f12 return] 'shell)
  (global-set-key [f12 ?F] 'find-file-at-point)
  (global-set-key [find] 'beginning-of-buffer) ;Home
  (global-set-key [select] 'end-of-buffer) ;End
  (global-set-key [delete] 'backward-delete-char-untabify)
  (global-set-key [kp-1] 'backward-word)
  (global-set-key [kp-3] 'forward-word)
  (global-set-key [(meta escape) \[ \5 \~] 'scroll-other-window-down) ;Alt-PgUp
  (global-set-key [(meta escape) \[ \6 \~] 'scroll-other-window) ;Alt-PgDn
  (global-set-key [(meta prior)] 'scroll-other-window-down) ;Alt-PgUp
  (global-set-key [(meta next)] 'scroll-other-window) ;Alt-PgDn

  ;; In absence of autoload machinery, load stuff directly
  (if (memq chah-location '(pepperpot mythic))
      (load "chah-ftp" nil t))
)
