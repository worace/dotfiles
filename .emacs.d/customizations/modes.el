(setq-default indent-tabs-mode nil)

;; Enable helm autofilter/complete interface
(require 'helm-config)
;; Use helm-M-x as default finder in M-x
(global-set-key (kbd "M-x") 'helm-M-x)
;; Additionally, enable helm for file-finding
;; And some other standard uses
(helm-mode 1)

;; Set up evil leader
;; Make sure to enable this before evil-mode
(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")

;; Evil (vim) Mode
(require 'evil)
(evil-mode 1)

;; Was losing evil gg in dired mode for some reason
;; probably using dired wrong but this fixes it...
(evil-define-key 'normal dired-mode-map "gg" 'beginning-of-buffer)

;; rainbow delims!
(rainbow-delimiters-mode)

;; Clojure Setup
(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
(require 'smartparens-config)
(smartparens-global-mode t)
(add-hook 'clojure-mode-hook #'smartparens-strict-mode)
(setq cider-show-error-buffer nil)
(defun cider-setup ()
  (setq show-trailing-whitespace nil)
  (setq truncate-lines nil))
(add-hook 'cider-repl-mode-hook #'cider-setup)

;; Markdown Setup
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; (defun md-setup ()
;;   (message "MARKDOWN SETUP RUNNING")
;;   (setq evil-cross-lines t)
;;   (visual-line-mode)
;;   (toggle-word-wrap))

(define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)

(add-hook 'markdown-mode-hook
          (lambda ()
            (setq evil-cross-lines t)
            (visual-line-mode)
            (toggle-word-wrap)))

(global-auto-complete-mode)
(global-evil-surround-mode 1)

;; Javascript mode
(setq js-indent-level 2)
(setq jsx-indent-level 2)
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . jsx-mode))
(autoload 'jsx-mode "jsx-mode" "JSX mode" t)


;;Hub Github Addon
;;Currently just installed locally
(require 'hub)

;;Emacs Livedown
;;Installed manually from here https://github.com/shime/emacs-livedown
(require 'livedown)

;; Haskell
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

;;ruby
(require 'inf-ruby)
(add-hook 'enh-ruby-mode-hook 'inf-ruby-minor-mode)
(when (executable-find "pry")
    (add-to-list 'inf-ruby-implementations '("pry" . "pry"))
    (setq inf-ruby-default-implementation "pry"))
(require 'ruby-mode)

;;Fireplace :)
(require 'fireplace)

(require 'web-mode)
(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.scss\\'" . web-mode))
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))
(add-hook 'web-mode-hook  'my-web-mode-hook)


;; Save Recent Files
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
