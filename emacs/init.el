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
    ("10e231624707d46f7b2059cc9280c332f7c7a530ebc17dba7e506df34c5332c4" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" default)))
 '(markdown-command "/usr/local/bin/markdown")
 '(package-selected-packages
   (quote
    (alchemist elixir-mode sass-mode racket-mode haml-mode logstash-conf typescript-mode gruvbox-theme toml-mode exec-path-from-shell cargo racer flycheck-rust coffee-mode yaml-mode web-mode virtualenvwrapper spotify solarized-theme smartparens seeing-is-believing ruby-test-mode request restclient rainbow-mode rainbow-delimiters neotree origami org-present org-bullets nose mocha markdown-toc markdown-mode magit jsx-mode json-reformat jade inf-ruby helm-projectile helm-circe helm-ag helm haskell-mode groovy-mode gist flycheck evil-surround evil-leader evil dockerfile-mode clj-refactor circe cider chruby auto-complete ag ace-window ace-jump-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 0.95))))
 '(org-document-title ((t (:inherit default :weight bold :foreground "#657b83" :font "Avenir Medium" :height 1.2 :underline nil))))
 '(org-level-1 ((t (:inherit default :weight bold :foreground "#657b83" :font "Avenir Medium" :height 1.2))))
 '(org-level-2 ((t (:inherit default :weight bold :foreground "#657b83" :font "Avenir Medium" :height 1.2))))
 '(org-level-3 ((t (:inherit default :weight bold :foreground "#657b83" :font "Avenir Medium" :height 1.2))))
 '(org-level-4 ((t (:inherit default :weight bold :foreground "#657b83" :font "Avenir Medium" :height 1.2))))
 '(org-level-5 ((t (:inherit default :weight bold :foreground "#657b83" :font "Avenir Medium" :height 1.2))))
 '(org-level-6 ((t (:inherit default :weight bold :foreground "#657b83" :font "Avenir Medium" :height 1.2))))
 '(org-level-7 ((t (:inherit default :weight bold :foreground "#657b83" :font "Avenir Medium" :height 1.2))))
 '(org-level-8 ((t (:inherit default :weight bold :foreground "#657b83" :font "Avenir Medium" :height 1.2)))))
(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
