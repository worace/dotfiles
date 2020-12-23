;; Set user info

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq user-full-name "Horace Williams")
(setq user-mail-address "horace.d.williams@gmail.com")


;; raise GC threshold to 100 MB
(setq gc-cons-threshold 100000000)

(add-to-list 'load-path "~/.emacs.d/customizations/")
(add-to-list 'load-path "~/.emacs.d/customizations/vendor")
(load "packages.el")
(load "ui.el")
(load "theme.el")
(load "modes.el")
(load "keybindings.el")
(load "drakefile-mode.el")

(setq exec-path-from-shell-check-startup-files nil)
(exec-path-from-shell-initialize)

(setq use-package-always-defer t
      use-package-always-ensure t)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq create-lockfiles nil)

(setq backup-by-copying t)
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 2)
 '(custom-safe-themes
   (quote
    ("cd4d1a0656fee24dc062b997f54d6f9b7da8f6dc8053ac858f15820f9a04a679" "62c81ae32320ceff5228edceeaa6895c029cc8f43c8c98a023f91b5b339d633f" "a5956ec25b719bf325e847864e16578c61d8af3e8a3d95f60f9040d02497e408" "f27c3fcfb19bf38892bc6e72d0046af7a1ded81f54435f9d4d09b3bff9c52fc1" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(markdown-command (executable-find "markdown"))
 '(package-selected-packages
   (quote
    (handlebars-mode sbt-mode scala-mode enh-ruby-mode terraform-mode lsp-java yaml-mode web-mode vue-mode virtualenvwrapper use-package toml-mode tide thrift swift-mode spotify solarized-theme smartparens seeing-is-believing ruby-test-mode ripgrep rinari rg restclient rainbow-mode rainbow-delimiters racket-mode racer protobuf-mode prettier-js play-routes-mode paredit origami org-present org-bullets nose neotree multiple-cursors mocha markdown-toc magit jsx-mode json-mode jade-mode hydra helm-rg helm-projectile helm-circe helm-ag haskell-mode haml-mode gruvbox-theme groovy-mode graphql-mode gist flycheck-swift flycheck-rust exec-path-from-shell evil-surround evil-leader emmet-mode edn dockerfile-mode csv-mode company-sourcekit company-rtags company-lsp company-irony company-go coffee-mode cmake-mode cider chruby cargo auto-complete alchemist ag ace-window ace-jump-mode)))
 '(python-guess-indent nil)
 '(python-indent 2)
 '(python-indent-guess-indent-offset nil)
 '(python-indent-offset 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 0.95)))))
