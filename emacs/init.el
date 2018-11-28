;; Set user info

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq user-full-name "Horace Williams")
(setq user-mail-address "horace.d.williams@gmail.com")

;; (setenv "PATH"
;;         (concat (getenv "PATH")
;;                 ":/usr/local/bin:$HOME/.cargo/bin"))
;; (setq exec-path (append exec-path '("/usr/local/bin" "$HOME/.cargo/bin")))


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
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(markdown-command (executable-find "markdown"))
 '(package-selected-packages
   (quote
    (company-lsp lsp-vue lsp-mode tide vue-mode handlebars-mode thrift org-ref swift-mode flycheck-swift company-sourcekit yaml-mode web-mode virtualenvwrapper use-package toml-mode spotify solarized-theme smartparens seeing-is-believing ruby-test-mode rinari restclient request rainbow-mode rainbow-delimiters racket-mode racer origami org-present org-bullets nose neotree mocha markdown-toc magit jsx-mode json-reformat json-mode jade-mode helm-projectile helm- helm-ag haskell-mode haml-mode gruvbox-theme groovy-mode gist flycheck-rust exec-path-from-shell evil-surround evil-leader ensime dockerfile-mode company-rtags company-irony company-go coffee-mode cmake-mode clj-refactor chruby cargo auto-complete alchemist ag ace-window ace-jump-mode)))
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
