;; Set user info
(setq user-full-name "Horace Williams")
(setq user-mail-address "horace.d.williams@gmail.com")

;; Disable Extra Status/Toolbars
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

;; Use y/n instead of yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

;; Set Startup Options -- Skip splash screen,
;; Give empty scratch buffer, and start in markdown mode
(setq inhibit-splash-screen t
      initial-scratch-message nil
      )
;;initial-major-mode 'markdown-mode


;; Set up emacs package system and add a few extra
;; Repositories
(load "package")
(package-initialize)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; Define list of packages to install
(defvar worace/packages '(evil) "Default Packages")

;; Require the common-lisp emacs extension; will use this
;; To use some CL-style macros in following config functions
(require 'cl)

;; Check if all our packages are installed; if we find one missing,
;; return nil
(defun worace/all-packages-installed ()
  (loop for pkg in worace/packages
	when (not (package-installed-p pkg)) do (return nil)
	finally (return t)))

;; If there are packages missing, install them
;; This will run every time on boot
(unless (worace/all-packages-installed)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg worace/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

;; Evil (vim) Mode
(require 'evil)
(evil-mode 1)

;; Typography
(set-face-attribute 'default nil
		    :family "Source Code Pro"
		    :height 150
		    :weight 'normal
		    :width 'normal)
;; Set user info
(setq user-full-name "Horace Williams")
(setq user-mail-address "horace.d.williams@gmail.com")

;; Disable Extra Status/Toolbars
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

;; Set Startup Options -- Skip splash screen,
;; Give empty scratch buffer, and start in markdown mode
(setq inhibit-splash-screen t
      initial-scratch-message nil
      )
;;initial-major-mode 'markdown-mode


;; Set up emacs package system and add a few extra
;; Repositories
(load "package")
(package-initialize)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; Define list of packages to install
(defvar worace/packages '(evil) "Default Packages")

;; Require the common-lisp emacs extension; will use this
;; To use some CL-style macros in following config functions
(require 'cl)

;; Check if all our packages are installed; if we find one missing,
;; return nil
(defun worace/all-packages-installed ()
  (loop for pkg in worace/packages
	when (not (package-installed-p pkg)) do (return nil)
	finally (return t)))

;; If there are packages missing, install them
;; This will run every time on boot
(unless (worace/all-packages-installed)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg worace/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

;; Evil (vim) Mode
(require 'evil)
(evil-mode 1)

;; Typography
(set-face-attribute 'default nil
		    :family "Source Code Pro"
		    :height 150
		    :weight 'normal
		    :width 'normal)

;; Add vim-style tilde gutter when file is empty
(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))

;; Tabs and Indentation Configuration
(setq tab-width 2
      indent-tabs-mode nil)

;; Configure Backup Files - Put them all in home dir
(setq backup-directory-alist `(("." . "~/.emacs_backups")))
(setq backup-by-copying t)
(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)

;; Reduce time until outputting keystrokes in minifbuffer
;; Disable dialog boxes
(setq echo-keystrokes 0.1
      use-dialog-box nil
      visible-bell 'top-bottom)

;; Use visible "dings" (instead of audible ones)
(defun worace/mode-line-visual-bell ()
  "A friendlier visual bell effect."
  (invert-face 'mode-line)
  (run-with-timer 0.05 nil 'invert-face 'mode-line))

(setq visible-bell nil
      ring-bell-function 'worace/mode-line-visual-bell)

;; Highlight matching parens
(show-paren-mode t)
