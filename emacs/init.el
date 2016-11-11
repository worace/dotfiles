;; Set user info

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq user-full-name "Horace Williams")
(setq user-mail-address "horace.d.williams@gmail.com")

(setq exec-path (append exec-path '("/usr/local/bin")))
(setq exec-path (append exec-path '("/usr/local/go/bin")))

(setenv "PATH"
        (concat (getenv "PATH")
                ":/usr/local/bin"))


;; raise GC threshold to 100 MB
(setq gc-cons-threshold 100000000)

(add-to-list 'load-path "~/.emacs.d/customizations/")
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
 '(custom-safe-themes
   (quote
    ("c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" default)))
 '(markdown-command "/usr/local/bin/markdown")
 '(package-selected-packages
   (quote
    (lua-mode flymake-go go-mode smart-mode-line-powerline-theme smart-mode-line powerline alert solarized-theme yaml-mode web-mode virtualenvwrapper spotify smartparens seeing-is-believing ruby-test-mode restclient request rainbow-mode rainbow-delimiters pyvenv pytest pabbrev origami org-present org-bullets nvm nose neotree mocha markdown-toc magit libmpdee jsx-mode json-reformat js2-refactor jade inf-ruby highlight-indentation helm-spotify helm-projectile helm-circe helm-ag haskell-mode groovy-mode gist flymake-python-pyflakes flycheck exec-path-from-shell evil-surround evil-leader dockerfile-mode csv-mode color-theme-solarized clj-refactor chruby auto-complete ag ace-window ace-jump-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
