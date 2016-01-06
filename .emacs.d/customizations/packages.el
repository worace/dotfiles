;; Set up emacs package system and add a few extra
;; Repositories
(load "package")
(package-initialize)
(add-to-list 'package-archives
 	     '("marmalade" . "http://marmalade-repo.org/packages/"))

(add-to-list 'package-archives
	     '("melpa-stable" . "http://stable.melpa.org/packages/")
	      t)

(add-to-list 'package-archives
	     '("melpa-stable-milkbox" . "http://melpa-stable.milkbox.net/packages/")
	      t)

(add-to-list 'package-archives
 	     '("melpa" . "https://melpa.org/packages/")
 	      t)

;; Define list of packages to install
(defvar worace/packages '(;;elm-mode
			  ag
			  default-text-scale
			  evil
			  evil-leader
			  evil-surround
			  ace-window
			  magit
			  gist
                          haskell-mode
			  helm
			  helm-ag
			  json-reformat
			  neotree
			  cider
			  clojure-mode
			  clj-refactor
			  projectile
			  ace-jump-mode
			  helm-projectile
			  smartparens
			  rainbow-delimiters
			  markdown-mode
			  markdown-toc
			  web-mode
			  jsx-mode
			  enh-ruby-mode
			  inf-ruby
			  auto-complete
			  solarized-theme))

(when (boundp 'package-pinned-packages)
  (setq package-pinned-packages
	'((cider              . "melpa-stable")
	  (clj-refactor       . "melpa-stable"))))


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
