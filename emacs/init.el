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
  (global-evil-leader-mode)
  (evil-leader/set-key "q" 'evil-quit)
  (evil-leader/set-key "k" 'kill-this-buffer)
  (evil-leader/set-key "s" 'save-buffer)
  (evil-leader/set-key "d" 'dired)
  (evil-leader/set-key "/" 'comment-or-uncomment-region)
  )

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
  (evil-define-key 'normal dired-mode-map "gg" 'beginning-of-buffer)
  (evil-define-key 'normal global-map "G" 'end-of-buffer)
  ;; try to stop me from constantly opening the stupid evil mode keymap help window...
  (define-key evil-normal-state-map (kbd "C-w C-h") 'evil-window-left))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

;; raise GC threshold to 100 MB
(setq gc-cons-threshold 100000000)

(add-to-list 'load-path "~/.emacs.d/customizations/")
(add-to-list 'load-path "~/.emacs.d/customizations/vendor")
(load "theme.el")
(load "ui.el")

(load "packages.el")
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
