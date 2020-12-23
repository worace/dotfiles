;; Package bootstrapping
(require 'package)
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(require 'use-package-ensure)
(setq use-package-always-ensure t)


(use-package evil-leader
  :config
  (evil-leader/set-leader "<SPC>")
  (global-evil-leader-mode))

(use-package evil
  :config
  (define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
  (define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
  (define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
  ;; Make evil treat language-dependent "symbols" as full words
  ;; e.g. move past underscores for ruby as part of same word
  (defalias #'forward-evil-word #'forward-evil-symbol)
  (evil-mode)
  ;; Was losing evil gg in dired mode for some reason
  ;; probably using dired wrong but this fixes it...
  (evil-define-key 'normal dired-mode-map "gg" 'beginning-of-buffer))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

;; raise GC threshold to 100 MB
(setq gc-cons-threshold 100000000)

(add-to-list 'load-path "~/.emacs.d/customizations/")
(add-to-list 'load-path "~/.emacs.d/customizations/vendor")
(load "packages.el")
(load "ui.el")
(load "theme.el")
(load "modes.el")
(load "keybindings.el")

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
   '("b89ae2d35d2e18e4286c8be8aaecb41022c1a306070f64a66fd114310ade88aa" "c433c87bd4b64b8ba9890e8ed64597ea0f8eb0396f4c9a9e01bd20a04d15d358" "e1d09f1b2afc2fed6feb1d672be5ec6ae61f84e058cb757689edb669be926896" "939ea070fb0141cd035608b2baabc4bd50d8ecc86af8528df9d41f4d83664c6a" "aded61687237d1dff6325edb492bde536f40b048eab7246c61d5c6643c696b7f" "4cf9ed30ea575fb0ca3cff6ef34b1b87192965245776afa9e9e20c17d115f3fb" "123a8dabd1a0eff6e0c48a03dc6fb2c5e03ebc7062ba531543dfbce587e86f2a" "a06658a45f043cd95549d6845454ad1c1d6e24a99271676ae56157619952394a" "cd4d1a0656fee24dc062b997f54d6f9b7da8f6dc8053ac858f15820f9a04a679" "62c81ae32320ceff5228edceeaa6895c029cc8f43c8c98a023f91b5b339d633f" "a5956ec25b719bf325e847864e16578c61d8af3e8a3d95f60f9040d02497e408" "f27c3fcfb19bf38892bc6e72d0046af7a1ded81f54435f9d4d09b3bff9c52fc1" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default))
 '(markdown-command (executable-find "markdown"))
 '(package-selected-packages
   '(ido handlebars-mode sbt-mode scala-mode enh-ruby-mode terraform-mode lsp-java yaml-mode web-mode vue-mode virtualenvwrapper use-package toml-mode tide thrift swift-mode spotify solarized-theme smartparens seeing-is-believing ruby-test-mode ripgrep rinari rg restclient rainbow-mode rainbow-delimiters racket-mode racer protobuf-mode prettier-js play-routes-mode paredit origami org-present org-bullets nose neotree multiple-cursors mocha markdown-toc magit jsx-mode json-mode jade-mode hydra helm-rg helm-projectile helm-circe helm-ag haskell-mode haml-mode gruvbox-theme groovy-mode graphql-mode gist flycheck-swift flycheck-rust exec-path-from-shell evil-surround evil-leader emmet-mode edn dockerfile-mode csv-mode company-sourcekit company-rtags company-lsp company-irony company-go coffee-mode cmake-mode cider chruby cargo auto-complete alchemist ag ace-window ace-jump-mode))
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
