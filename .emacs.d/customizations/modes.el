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

(custom-set-variables
 '(markdown-command "/usr/local/bin/markdown"))

;; (defun md-setup ()
;;   (message "MARKDOWN SETUP RUNNING")
;;   (setq evil-cross-lines t)
;;   (visual-line-mode)
;;   (toggle-word-wrap))

(define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)

;; (add-hook 'markdown-mode-hook 'visual-line-mode)
(add-hook 'markdown-mode-hook
          (lambda ()
            (setq evil-cross-lines t)
            (visual-line-mode 1)
            ;; (toggle-word-wrap)
            ))

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

;;Exercism
(require 'request) ;; needed for exercism
(require 'exercism)

;; OOOOOORG
;; Don't show // around italics
(setq org-hide-emphasis-markers t)
;; Have org treat code blocks like their native lang
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
;; No line numbers in org (looks weird with the different sized headers)
(add-hook 'org-mode-hook (lambda () (global-linum-mode 0)))
;;Evil Bindings
(evil-leader/set-key-for-mode 'org-mode "o" 'org-open-at-point)

;; use pretty unicode bullets for lists
(font-lock-add-keywords 'org-mode
                        '(("^ +\\([-*]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))


(let* ((variable-tuple (cond ((x-list-fonts "Avenir Medium") '(:font "Avenir Medium"))
                             ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
                             (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
       (base-font-color     (face-foreground 'default nil 'default))
       (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

  (custom-theme-set-faces 'user
                          `(org-level-8 ((t (,@headline ,@variable-tuple))))
                          `(org-level-7 ((t (,@headline ,@variable-tuple))))
                          `(org-level-6 ((t (,@headline ,@variable-tuple))))
                          `(org-level-5 ((t (,@headline ,@variable-tuple))))
                          `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
                          `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
                          `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
                          `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
                          `(org-document-title ((t (,@headline ,@variable-tuple :height 1.5 :underline nil))))))
