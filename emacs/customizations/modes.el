(setq-default indent-tabs-mode nil)

(global-linum-mode 1)

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

;; Magit allow longer commit lines than the default
(setq git-commit-summary-max-length 70)

;;Emacs Livedown
;;Installed manually from here https://github.com/shime/emacs-livedown
(require 'livedown)

;; Haskell
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

;;ruby
(require 'chruby)
(chruby "2.2.2")
(require 'seeing-is-believing)
(add-hook 'ruby-mode-hook 'seeing-is-believing)
(require 'inf-ruby)
(add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)
(when (executable-find "pry")
    (add-to-list 'inf-ruby-implementations '("pry" . "pry"))
    (setq inf-ruby-default-implementation "pry"))
(require 'ruby-test-mode)
(add-hook 'compilation-finish-functions
          (lambda (buf strg)
            (switch-to-buffer-other-window "*compilation*")
            (read-only-mode)
            (goto-char (point-max))
            (local-set-key (kbd "q")
                           (lambda () (interactive) (quit-restore-window)))))
(add-hook 'ruby-mode-hook 'ruby-test-mode)

(require 'ruby-mode)

;;;;;;;;;;;;;
;; Python ;;;
;;;;;;;;;;;;;

(defun python-shell-clear-output ()
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

(require 'nose)
(add-hook 'python-mode-hook (lambda () (nose-mode t)))

(require 'flycheck)
(add-hook 'python-mode-hook 'flycheck-mode)

(add-hook 'inferior-python-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-k")
                           'python-shell-clear-output)))


(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-auto-closing t)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

(add-hook 'web-mode-hook  'my-web-mode-hook)


;; Save Recent Files
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)

;;Exercism

(if (file-accessible-directory-p "~/.exercism.json")
    (progn
      (require 'request) ;; needed for exercism
      (require 'exercism)))

(add-to-list 'auto-mode-alist '("\\.txt\\'" . text-mode))
;; Text Mode (Org, Markdown, etc)
(defun worace-text-mode-hook ()
  (turn-on-visual-line-mode)
  (toggle-word-wrap 1)
  (setq evil-cross-lines t))
(add-hook 'text-mode-hook 'worace-text-mode-hook)


;; OOOOOORG
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
;; Don't show // around italics
(setq org-hide-emphasis-markers t)
;; Have org treat code blocks like their native lang
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
;; No line numbers in org (looks weird with the different sized headers)
(add-hook 'org-mode-hook (lambda () (linum-mode 0)))

;; use pretty unicode bullets for lists
(font-lock-add-keywords 'org-mode
                        '(("^ +\\([-*]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(add-hook 'org-mode-hook (lambda () (org-indent-mode 1)))
(setq org-src-preserve-indentation nil
      org-edit-src-content-indentation 0)

(defun org-insert-example-block ()
  (interactive)
  (progn
    (newline-and-indent)
    (insert "#+BEGIN_EXAMPLE\n")
    (newline-and-indent)
    (insert "#+END_SRC\n")
    (previous-line 2)
    (org-edit-src-code)))

(defun org-insert-src-block (src-code-type)
  "Insert a `SRC-CODE-TYPE' type source code block in org-mode."
  (interactive
   (let ((src-code-types
          '("emacs-lisp" "python" "C" "sh" "shell" "java" "js" "clojure" "C++" "css"
            "calc" "asymptote" "dot" "gnuplot" "ledger" "lilypond" "mscgen"
            "octave" "oz" "plantuml" "R" "sass" "screen" "sql" "awk" "ditaa"
            "haskell" "latex" "lisp" "matlab" "ocaml" "org" "perl" "ruby"
            "scheme" "sqlite")))
     (list (ido-completing-read "Source code type: " src-code-types))))
  (progn
    (newline-and-indent)
    (insert (format "#+BEGIN_SRC %s\n" src-code-type))
    (newline-and-indent)
    (insert "#+END_SRC\n")
    (previous-line 2)
    (org-edit-src-code)))

(defun worace-org-mode-setup ()
  ;; keybinding for inserting code blocks
  (local-set-key (kbd "C-c s i") 'org-insert-src-block)
  (let* ((variable-tuple (cond ((x-list-fonts "Avenir Medium") '(:font "Avenir Medium"))
                               ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
                               (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
         (base-font-color     (face-foreground 'default nil 'default))
         (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

    (custom-theme-set-faces 'user
                            `(org-level-8 ((t (,@headline ,@variable-tuple :height 1.2))))
                            `(org-level-7 ((t (,@headline ,@variable-tuple :height 1.2))))
                            `(org-level-6 ((t (,@headline ,@variable-tuple :height 1.2))))
                            `(org-level-5 ((t (,@headline ,@variable-tuple :height 1.2))))
                            `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.2))))
                            `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.2))))
                            `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.2))))
                            `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.2))))
                            `(org-document-title ((t (,@headline ,@variable-tuple :height 1.2 :underline nil)))))))

(add-hook 'org-mode-hook 'worace-org-mode-setup)

;;Evil Bindings
(evil-leader/set-key-for-mode 'org-mode "o" 'org-open-at-point)
(evil-leader/set-key-for-mode 'org-mode "i s" 'org-edit-src-code)
(evil-leader/set-key-for-mode 'org-mode "i l" 'org-insert-link)
(evil-leader/set-key-for-mode 'org-mode "i i" 'org-insert-list-item)

(require 'origami)
(global-origami-mode)

(require 'yaml-mode)


(require 'org-present)
(evil-leader/set-key-for-mode 'org-present-mode "<right>" 'org-present-next)
(evil-leader/set-key-for-mode 'org-present-mode "<left>" 'org-present-prev)
(setq org-image-actual-width '(400))
(eval-after-load "org-present"
  '(progn
     (add-hook 'org-present-mode-hook
               (lambda ()
                 (local-set-key (kbd "C-<right>") 'org-present-next)
                 (local-set-key (kbd "C-<left>") 'org-present-prev)
                 (org-display-inline-images)
                 (org-present-hide-cursor)
                 (org-present-read-only)))
     (add-hook 'org-present-mode-quit-hook
               (lambda ()
                 (org-remove-inline-images)
                 (org-present-show-cursor)
                 (org-present-read-write)))))
