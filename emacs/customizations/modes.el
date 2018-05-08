(if (file-exists-p "~/.secrets.el")
    (load "~/.secrets.el"))

(setq-default truncate-lines 1)
(setq-default indent-tabs-mode nil)
(setq company-backends '())

(ido-mode 1)
(ido-everywhere 1)

;;Projectile
(setq projectile-git-submodule-command "echo ''")

(global-linum-mode 1)

;; Enable helm autofilter/complete interface
(require 'helm-config)
;; Use helm-M-x as default finder in M-x
(global-set-key (kbd "M-x") 'helm-M-x)
;; Additionally, enable helm for file-finding
;; And some other standard uses
(helm-mode 1)

(setq helm-ag-use-agignore t)

;; Set up evil leader
;; Make sure to enable this before evil-mode
(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")

;; Evil (vim) Mode
(require 'evil)
(evil-mode 1)
;; Make evil treat language-dependent "symbols" as full words
;; e.g. move past underscores for ruby as part of same word
(with-eval-after-load 'evil
  (defalias #'forward-evil-word #'forward-evil-symbol))

;; Disable Evil for certain modes
(add-to-list 'evil-emacs-state-modes 'eshell-mode)

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
(setq cider-use-fringe-indicators nil)
(defun cider-setup ()
  (setq show-trailing-whitespace nil)
  (setq truncate-lines nil))
(add-hook 'cider-repl-mode-hook #'cider-setup)

;; Markdown Setup
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(sp-local-pair 'markdown-mode "`" nil :actions '(insert))

(custom-set-variables
 '(markdown-command (executable-find "markdown")))

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
(setq-default js2-global-externs
              '("module" "require" "sinon" "assert" "refute" "setTimeout"
                "clearTimeout" "setInterval" "clearInterval" "location"
                "__dirname" "console" "JSON" "describe" "it" "beforeEach"
                "before" "after" "afterEach"))

(setq js2-bounce-indent-p t
      js2-include-node-externs t
      js2-include-browser-externs t
      js2-basic-offset 2
      js2-mode-show-parse-errors nil
      js2-mode-show-strict-warnings nil
      js2-strict-trailing-comma-warning nil
      js2-strict-missing-semi-warning nil
      js2-strict-inconsistent-return-warning nil)

(setq js2-bounce-indent-p t)
(setq js2-basic-offset 2)
(setq js2-include-node-externs t)
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . js2-jsx-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-jsx-mode))
(autoload 'jsx-mode "jsx-mode" "JSX mode" t)

(add-hook 'js2-jsx-mode-hook
          (lambda ()
            (setq truncate-lines t)
            (toggle-truncate-lines nil)))

(setq-default flycheck-eslintrc "~/.eslintrc.json")
;; (setq flycheck-disabled-checkers
;;       (append flycheck-disabled-checkers
;;               '(javascript-jshint)))
;; (setq flycheck-checkers '(javascript-eslint))

(defun use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root)))
         (eslintrc (and root
                        (expand-file-name ".eslintrc.json"
                                          root))))
    (when eslintrc
      (setq-local flycheck-eslintrc eslintrc)
      (setq-local flycheck-eslint-rules-directories (list (expand-file-name root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

(add-hook 'js2-mode-hook
          (lambda ()
            (toggle-truncate-lines nil)
            (flycheck-mode t)
            (when (executable-find "eslint")
              (flycheck-select-checker 'javascript-eslint)
              (use-eslint-from-node-modules))))

(if (executable-find "nvm")
 (progn
  (require 'nvm)
  (defun do-nvm-use (version)
   (interactive "sVersion: ")
   (nvm-use version)
   (exec-path-from-shell-copy-env "PATH"))
  (nvm-use "6.5.0")))


(defun node-repl () (interactive)
       (pop-to-buffer (make-comint "node-repl" "node" nil "--interactive")))

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
(chruby "2.4.4")
(require 'seeing-is-believing)
(add-hook 'ruby-mode-hook 'seeing-is-believing)
(require 'inf-ruby)
(add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)
(when (executable-find "pry")
    (add-to-list 'inf-ruby-implementations '("pry" . "pry"))
    (setq inf-ruby-default-implementation "pry"))

(defun my-compilation-finish-hook (buf strg)
  (switch-to-buffer-other-window "*compilation*")
  (read-only-mode)
  (goto-char (point-max))
  (local-set-key (kbd "q")
                 (lambda () (interactive) (quit-restore-window))))
(add-hook 'compilation-finish-functions 'my-compilation-finish-hook nil nil)

(require 'ruby-test-mode)
(add-hook 'ruby-mode-hook 'ruby-test-mode)
(require 'ruby-mode)

;;;;;;;;;;;;;
;; Python ;;;
;;;;;;;;;;;;;

(defun python-shell-clear-output ()
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

(add-hook 'inferior-python-mode-hook
          (lambda ()
            (disable-trailing-whitespace)
            (local-set-key (kbd "C-c C-k")
                           'python-shell-clear-output)))

(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "--simple-prompt -i")

(setq-default py-shell-name "ipython")
(setq-default py-which-bufname "IPython")
(setq py-shell-switch-buffers-on-execute-p t)
(setq py-switch-buffers-on-execute-p t)
(setq py-smart-indentation t)

(custom-set-variables
 '(python-guess-indent nil)
 '(python-indent 2))


(require 'virtualenvwrapper)
(setq venv-location "~/.virtualenvs")


(require 'nose)
(add-hook 'python-mode-hook (lambda () (nose-mode t)))

(require 'flycheck)
(add-hook 'python-mode-hook 'flycheck-mode)


(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))

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
(setq recentf-max-menu-items 2000)
(recentf-mode 1)
(run-at-time nil (* 5 60) 'recentf-save-list)


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
;; (setq org-hide-emphasis-markers t)
;; Have org treat code blocks like their native lang
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
;; No line numbers in org (looks weird with the different sized headers)
;; (add-hook 'org-mode-hook (lambda () (linum-mode 0)))

;; use pretty unicode bullets for lists
(font-lock-add-keywords 'org-mode
                        '(("^ +\\([-*]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
;; (require 'org-bullets)
;; (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(use-package org-bullets
  :ensure t
  :init
  ;; org-bullets-bullet-list
  ;; default: "◉ ○ ✸ ✿"
  ;; large: ♥ ● ◇ ✚ ✜ ☯ ◆ ♠ ♣ ♦ ☢ ❀ ◆ ◖ ▶
  ;; Small: ► • ★ ▸
  (setq org-bullets-bullet-list '("•"))
  ;; others: ▼, ↴, ⬎, ⤷,…, and ⋱.
  ;; (setq org-ellipsis "⤵")
  (setq org-ellipsis "…")
  :config
  (add-hook 'org-mode-hook #'org-bullets-mode))


(add-hook 'org-mode-hook (lambda () (org-indent-mode 1)))
(setq org-src-preserve-indentation nil
      org-edit-src-content-indentation 0)

(defun org-insert-example-block ()
  (interactive)
  (progn
    (newline-and-indent)
    (insert "#+BEGIN_EXAMPLE\n")
    (newline-and-indent)
    (insert "#+END_EXAMPLE\n")
    (previous-line 2)
    (org-edit-src-code)))

(defun org-insert-src-block (src-code-type)
  "Insert a `SRC-CODE-TYPE' type source code block in org-mode."
  (interactive
   (let ((src-code-types
          '("emacs-lisp" "python" "C" "sh" "shell" "html" "java" "js" "json" "clojure" "C++" "css"
            "octave" "sass" "scala" "sql" "awk" "haskell" "lisp"
            "org" "racket" "ruby" "scheme")))
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
  (local-set-key (kbd "C-c s e") 'org-insert-example-block)
  (let* ((variable-tuple (cond ((x-family-fonts "Sans Serif") '(:family "Sans Serif"))
                               (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
         (base-font-color     (face-foreground 'default nil 'default))
         (headline           `(:inherit default :foreground ,base-font-color)))

    ;; (custom-theme-set-faces 'user
    ;;                         `(org-level-8 ((t (,@headline ,@variable-tuple))))
    ;;                         `(org-level-7 ((t (,@headline ,@variable-tuple))))
    ;;                         `(org-level-6 ((t (,@headline ,@variable-tuple))))
    ;;                         `(org-level-5 ((t (,@headline ,@variable-tuple))))
    ;;                         `(org-level-4 ((t (,@headline :foreground "#bdbdb3" ,@variable-tuple))))
    ;;                         `(org-level-3 ((t (,@headline :foreground "#bdbdb3" ,@variable-tuple))))
    ;;                         `(org-level-2 ((t (,@headline :foreground "#bdbdb3" ,@variable-tuple))))
    ;;                         `(org-level-1 ((t (,@headline :foreground "#bdbdb3" ,@variable-tuple))))
    ;;                         `(org-document-title ((t (,@headline ,@variable-tuple :color "#bdbdb3" :underline nil)))))
    ))

(add-hook 'org-mode-hook 'worace-org-mode-setup)

;;Evil Bindings
(evil-leader/set-key-for-mode 'org-mode "o" 'org-open-at-point)
(evil-leader/set-key-for-mode 'org-mode "i s" 'org-edit-src-code)
(evil-leader/set-key-for-mode 'org-mode "i l" 'org-insert-link)
(evil-leader/set-key-for-mode 'org-mode "i i" 'org-insert-list-item)

;; Configure additional languages for org mode
;;(require 'ob-racket)
;;(org-babel-do-load-languages
;; 'org-babel-load-languages
;; '((racket . t)))


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

(add-to-list 'auto-mode-alist '("\\.el\\'" . emacs-lisp-mode))
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)


;; Postgres / SQL Mode
;; sql-connection-alist defined in ~/.secrets.el
(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (toggle-truncate-lines t)
            (disable-trailing-whitespace)))


(custom-set-variables '(coffee-tab-width 2))

(add-hook 'rust-mode-hook 'cargo-minor-mode)

(setq tramp-default-method "ssh")

(require 'circe)
(evil-set-initial-state 'circe-mode 'emacs)

(add-to-list 'circe-networks '("Mozilla"  :host "irc.mozilla.org"  :port (6667 . 6697)))

(add-hook 'circe-mode-hook
          (lambda ()
            (setq show-trailing-whitespace nil)))

;; irc-nick, irc-pass loaded from ~/.secrets.el
(if (boundp 'irc-nick)
    (setq circe-network-options
          `(("Mozilla"
             :nick ,irc-nick
             :channels ("#rust" "#rust-beginners")
             :nickserv-password ,irc-pass))))

(setq sh-basic-offset 2
      sh-indentation 2)

;; Octave Setup
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))
(evil-leader/set-key-for-mode 'octave-mode "e b" 'octave-send-buffer)
(evil-leader/set-key-for-mode 'octave-mode "e r" 'octave-send-region)
(evil-leader/set-key-for-mode 'inferior-octave-mode "k" 'comint-clear-buffer)
(defun inferior-octave-setup ()
  (setq show-trailing-whitespace nil))
(add-hook 'inferior-octave-mode-hook #'inferior-octave-setup)

(custom-set-faces
 '(aw-leading-char-face
   ((t (:inherit ace-jump-face-foreground :height 0.95)))))


(setq racket-racket-program "/Applications/Racket v6.8/bin/racket")
(setq racket-raco-program "/Applications/Racket v6.8/bin/raco")
(setq org-babel-racket-command "/Applications/Racket v6.8/bin/racket")
(evil-leader/set-key-for-mode 'racket-mode "e b" 'racket-run)
(evil-leader/set-key-for-mode 'racket-mode "e r" 'racket-send-region)
(evil-leader/set-key-for-mode 'racket-repl-mode "k" 'comint-clear-buffer)
(evil-leader/set-key-for-mode 'racket-mode "ps" 'sp-forward-slurp-sexp)
(evil-leader/set-key-for-mode 'racket-mode "pb" 'sp-forward-barf-sexp)
(evil-leader/set-key-for-mode 'racket-mode "pp" 'sp-splice-sexp-killing-around)
(defun racket-repl-setup ()
  (setq show-trailing-whitespace nil)
  (setq truncate-lines nil))
(add-hook 'racket-repl-mode-hook #'racket-repl-setup)


;; Elixir
(add-hook 'alchemist-iex-mode-hook
          (lambda ()
            (setq show-trailing-whitespace nil)))
(evil-leader/set-key-for-mode 'elixir-mode "eb" 'alchemist-execute-this-buffer)
(evil-leader/set-key-for-mode 'elixir-mode "er" 'alchemist-send-region)

(sp-with-modes '(elixir-mode)
  (sp-local-pair "fn" "end"
         :when '(("SPC" "RET"))
         :actions '(insert navigate))
  (sp-local-pair "do" "end"
         :when '(("SPC" "RET"))
         :post-handlers '(sp-ruby-def-post-handler)
         :actions '(insert navigate)))

(add-hook 'java-mode-hook (lambda ()
                            (setq c-basic-offset 2)
                            (setq truncate-lines t)))


;; C/CPP
;; http://syamajala.github.io/c-ide.html
(require 'rtags)
(require 'company-rtags)

;; (setq rtags-completions-enabled t)
;; (eval-after-load 'company
;;   '(add-to-list
;;     'company-backends 'company-rtags))
;; (setq rtags-autostart-diagnostics t)
;; (rtags-enable-standard-keybindings)
;; (setq rtags-display-result-backend 'helm)
(require 'irony)

(defun my-c-mode-hook ()
  (irony-mode)
  (company-mode)
  (flycheck-mode)
  (setq tab-width 2)
  (setq c-basic-offset 2)
  (setq flycheck-clang-include-path (list "/usr/local/include"))
  (company-irony-setup-begin-commands)
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))

(add-hook 'c++-mode-hook 'my-c-mode-hook)
(add-hook 'c-mode-hook 'my-c-mode-hook)
(add-hook 'objc-mode-hook 'my-c-mode-hook)

(setq company-backends (delete 'company-semantic company-backends))
(eval-after-load 'company
  '(add-to-list
    'company-backends 'company-irony))
(setq company-idle-delay 0)
(define-key c-mode-map [(tab)] 'company-complete)
(define-key c++-mode-map [(tab)] 'company-complete)
(define-key objc-mode-map [(tab)] 'company-complete)

;; Golang
(defun worace-go-mode-hook ()
  (set (make-local-variable 'company-backends) '(company-go))
  (company-mode)
  (auto-complete-mode -1)
  (flycheck-mode))
(add-hook 'go-mode-hook 'worace-go-mode-hook)

;; Company Mode
(setq company-tooltip-limit 20)                      ; bigger popup window
(setq company-idle-delay .3)                         ; decrease delay before autocompletion popup shows
(setq company-echo-delay 0)                          ; remove annoying blinking
(setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing

;; Swift / XCode
(eval-after-load 'flycheck '(flycheck-swift-setup))
(setq flycheck-swift-sdk-path "/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS.sdk")
(setq flycheck-swift-target "arm64-apple-ios10")
(require 'company-sourcekit)
(add-to-list 'company-backends 'company-sourcekit)
(setq sourcekit-sourcekittendaemon-executable
      "/usr/local/bin/sourcekittendaemon")

(setq org-export-allow-bind-keywords 't)
(setq org-html-preamble 'nil)
(setq org-html-preamble-format 'nil)
