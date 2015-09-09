;; Set up emacs package system and add a few extra
;; Repositories
(load "package")
(package-initialize)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; Define list of packages to install
(defvar worace/packages '(evil
			  evil-leader
			  magit
			  gist
			  helm
			  helm-ag
			  neotree
			  cider
			  clojure-mode
			  projectile
			  ace-jump-mode
			  helm-projectile
			  smartparens
			  rainbow-delimiters
			  markdown-mode
			  web-mode
			  enh-ruby-mode
			  auto-complete
			  solarized-theme))

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
