;; Disable Extra Status/Toolbars
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)


(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)

;; Use y/n instead of yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

;; Set Startup Options -- Skip splash screen,
;; Give empty scratch buffer, and start in markdown mode
(setq inhibit-splash-screen t
      initial-scratch-message nil
      initial-major-mode 'markdown-mode)

;; Add vim-style tilde gutter when file is empty
(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))

(setq echo-keystrokes 0.1 ;; Reduce time until outputting keystrokes in minifbuffer
      use-dialog-box nil ;; Disable dialog boxes
      blink-cursor-mode nil ;;don't blink the cursor
      )

;; Use visible "dings" (instead of audible ones)
(defun worace/mode-line-visual-bell ()
  "A friendlier visual bell effect."
  (invert-face 'mode-line)
  (run-with-timer 0.05 nil 'invert-face 'mode-line))

(setq visible-bell nil
      ring-bell-function 'worace/mode-line-visual-bell)

;; Disable auto-line-wrapping (truncate instead)
(add-hook 'hack-local-variables-hook
	  (lambda () (setq truncate-lines t)))

(defadvice find-file (before make-directory-maybe (filename &optional wildcards) activate)
  "Create parent directory if not exists while visiting file."
  (unless (file-exists-p filename)
    (let ((dir (file-name-directory filename)))
      (unless (file-exists-p dir)
        (make-directory dir)))))
