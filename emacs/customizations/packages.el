;; Set up emacs package system and add a few extra
;; Repositories
(load "package")

;; (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
;; (setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
;; 			 ("melpa" . "https://melpa.org/packages/")
;; 			 ("melpa-stable" . "https://stable.melpa.org/packages/")))

(use-package dash)

;; Define list of packages to install
(defvar worace/packages '(auto-complete
                          cargo
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
                          gist
                          graphql-mode
                          groovy-mode
                          gruvbox-theme
                          haml-mode
                          helm-projectile
                          helm-rg
                          jade-mode
                          json-mode
                          json-reformat
                          markdown-toc
                          mocha
                          ;;nose
                          org-bullets
                          neotree
                          play-routes-mode
                          prettier-js
                          protobuf-mode
                          rainbow-mode
                          restclient
                          request
                          rg
                          solarized-theme
                          spotify
                          toml-mode
                          thrift
                          use-package
                          virtualenvwrapper
                          web-mode
                          yaml-mode))

(when (boundp 'package-pinned-packages)
  (setq package-pinned-packages
	'((cider              . "melpa-stable")
    (yaml-mode          . "melpa-stable")
    (helm-config          . "melpa-stable")
    (clj-refactor       . "melpa-stable"))))
