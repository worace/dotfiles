;; Set up emacs package system and add a few extra
;; Repositories
(load "package")

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")
			 ("melpa-stable" . "https://stable.melpa.org/packages/")
       ))

(use-package dash)

;; Define list of packages to install
(defvar worace/packages '(ace-jump-mode
                          ace-window
                          alchemist
                          auto-complete
                          cargo
                          chruby
                          cider
                          clojure-mode
                          cmake-mode
                          coffee-mode
                          dash
                          dash-functional
                          dockerfile-mode
                          elixir-mode
                          emmet-mode
                          evil
                          evil-leader
                          evil-surround
                          exec-path-from-shell
                          flycheck
                          flycheck-rust
                          gist
                          graphql-mode
                          groovy-mode
                          gruvbox-theme
                          haml-mode
                          haskell-mode
                          helm
                          helm-projectile
                          helm-rg
                          inf-ruby
                          jade-mode
                          json-mode
                          json-reformat
                          ;;jsx-mode
                          js2-mode
                          lsp-mode
                          lsp-ui
                          magit
                          markdown-mode
                          markdown-toc
                          mocha
                          ;;nose
                          org-bullets
                          org-present
                          origami
                          neotree
                          play-routes-mode
                          prettier-js
                          protobuf-mode
                          projectile
                          racer
                          rainbow-delimiters
                          rainbow-mode
                          restclient
                          request
                          rg
                          rinari
                          ruby-test-mode
                          rust-mode
                          sbt-mode
                          scala-mode
                          seeing-is-believing
                          smartparens
                          solarized-theme
                          spotify
                          toml-mode
                          thrift
                          tide
                          use-package
                          virtualenvwrapper
                          web-mode
                          yaml-mode))

(when (boundp 'package-pinned-packages)
  (setq package-pinned-packages
	'((cider              . "melpa-stable")
          (yaml-mode          . "melpa-stable")
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
