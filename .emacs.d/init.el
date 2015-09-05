;; Set user info
(setq user-full-name "Horace Williams")
(setq user-mail-address "horace.d.williams@gmail.com")

;; Disable Extra Status/Toolbars
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

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
