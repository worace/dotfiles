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

(defun md-setup () (setq truncate-lines nil))
(add-hook 'markdown-mode-hook #'md-setup)

(global-auto-complete-mode)
(global-evil-surround-mode 1)

;; Javascript mode
(setq js-indent-level 2)
(setq jsx-indent-level 2)
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . jsx-mode))
(autoload 'jsx-mode "jsx-mode" "JSX mode" t)


;;Hub Github Addon
;;Currently just installed locally
;;(require 'hub)

;; Haskell
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
