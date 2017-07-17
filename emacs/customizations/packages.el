;; Set up emacs package system and add a few extra
;; Repositories
(load "package")
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable-milkbox" . "http://melpa-stable.milkbox.net/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Define list of packages to install
(defvar worace/packages '(;;elm-mode
                          ace-jump-mode
                          ace-window
                          alchemist
                          ag
                          auto-complete
                          cargo
                          chruby
                          cider
                          circe
                          clj-refactor
                          clojure-mode
                          coffee-mode
                          company-irony
                          company-rtags
                          dockerfile-mode
                          elixir-mode
                          ensime
                          evil
                          evil-leader
                          evil-surround
                          exec-path-from-shell
                          flycheck
                          flycheck-rust
                          gist
                          groovy-mode
                          gruvbox-theme
                          haml-mode
                          haskell-mode
                          helm
                          helm-ag
                          helm-circe
                          helm-projectile
                          inf-ruby
                          irony
                          jade-mode
                          json-mode
                          json-reformat
                          jsx-mode
                          js2-mode
                          magit
                          markdown-mode
                          markdown-toc
                          mocha
                          nose
                          org-bullets
                          org-present
                          origami
                          neotree
                          projectile
                          racer
                          racket-mode
                          rainbow-delimiters
                          rainbow-mode
                          restclient
                          request
                          rtags
                          ruby-test-mode
                          rust-mode
                          seeing-is-believing
                          smartparens
                          solarized-theme
                          spotify
                          toml-mode
                          use-package
                          virtualenvwrapper
                          web-mode
                          yaml-mode))

(when (boundp 'package-pinned-packages)
  (setq package-pinned-packages
	'((cider              . "melpa-stable")
          (circe              . "melpa-stable")
          (yaml-mode          . "melpa-stable")
	  (clj-refactor       . "melpa-stable")
          (rtags              . "melpa-stable"))))


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
