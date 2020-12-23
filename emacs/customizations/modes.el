(require 'use-package)
(if (file-exists-p "~/.secrets.el")
    (load "~/.secrets.el"))

(setq-default truncate-lines 1)
(setq-default indent-tabs-mode nil)

(ido-mode 1)

;;Projectile
(setq projectile-git-submodule-command nil)

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
;; Make evil treat language-dependent "symbols" as full words
;; e.g. move past underscores for ruby as part of same word
(with-eval-after-load 'evil
  (defalias #'forward-evil-word #'forward-evil-symbol))

;; Was losing evil gg in dired mode for some reason
;; probably using dired wrong but this fixes it...
(evil-define-key 'normal dired-mode-map "gg" 'beginning-of-buffer)

;; rainbow delims!
(rainbow-delimiters-mode)
(use-package rainbow-delimiters
  :ensure t)

;; Clojure Setup
(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
(require 'smartparens-config)
(smartparens-global-mode t)
(add-hook 'clojure-mode-hook #'smartparens-strict-mode)
(setq cljr-inject-dependencies-at-jack-in nil)
(setq cider-show-error-buffer nil)
(setq cider-use-fringe-indicators nil)
(setq cider-repl-history-size 1000)
(setq cider-repl-history-file "~/.ciderhistory")
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
(setq ruby-deep-arglist nil)
(setq ruby-deep-indent-paren nil)

(defun my-compilation-finish-hook (buf strg)
  (switch-to-buffer-other-window "*compilation*")
  (read-only-mode)
  (goto-char (point-max))
  (local-set-key (kbd "q")
                 (lambda () (interactive) (quit-restore-window))))

(add-hook 'ruby-mode-hook
          (lambda ()
            (add-hook 'compilation-finish-functions 'my-compilation-finish-hook nil 'make-it-local)))

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


(require 'flycheck)
(add-hook 'python-mode-hook 'flycheck-mode)


(require 'web-mode)
(use-package web-mode
  :ensure t
  :mode (".erb$" ".scss$" ".css$" ".html?$" ".hbs$" ".eex$")
  :config (setq web-mode-markup-indent-offset 2
                web-mode-enable-auto-pairing nil
                web-mode-enable-auto-closing t
                web-mode-css-indent-offset 2
                web-mode-code-indent-offset 2))

;; Support ERB/EEX tags in SmartParens
;; https://emacs.stackexchange.com/questions/15188/smartparens-and-web-mode-conflict-to-add-extra-angular-bracket
;; (sp-pair "%" "%" :wrap "C-%")
;; (sp-pair "<" ">" :wrap "C->")

;; Save Recent Files
(require 'recentf)
(setq recentf-max-menu-items 2000)
(recentf-mode 1)
(run-at-time nil (* 5 60) 'recentf-save-list)


(add-to-list 'auto-mode-alist '("\\.txt\\'" . text-mode))
;; Text Mode (Org, Markdown, etc)
(defun worace-text-mode-hook ()
  (turn-on-visual-line-mode)
  (toggle-word-wrap 1)
  (setq evil-cross-lines t))
(add-hook 'text-mode-hook 'worace-text-mode-hook)


(defun worace-org-mode-setup ()
  ;; keybinding for inserting code blocks
  (local-set-key (kbd "C-c s i") 'org-insert-src-block)
  (local-set-key (kbd "C-c s e") 'org-insert-example-block)
  (let* ((variable-tuple (cond ((x-family-fonts "Sans Serif") '(:family "Sans Serif"))
                               (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
         (base-font-color     (face-foreground 'default nil 'default))
         (headline           `(:inherit default :foreground ,base-font-color)))))

(use-package org
  :mode ("\\.org$" . org-mode)
  :ensure t
  :hook ((org-mode . worace-org-mode-setup)
         (org-mode . (lambda () (linum-mode 0))))
  :config (setq org-src-fontify-natively t
                inhibit-compacting-font-caches t
                org-startup-folded t
                org-src-tab-acts-natively t
                org-src-preserve-indentation nil
                org-edit-src-content-indentation 0))

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
            "sass" "scala" "sql" "awk" "haskell" "lisp"
            "org" "racket" "ruby" "scheme")))
     (list (ido-completing-read "Source code type: " src-code-types))))
  (progn
    (newline-and-indent)
    (insert (format "#+BEGIN_SRC %s\n" src-code-type))
    (newline-and-indent)
    (insert "#+END_SRC\n")
    (previous-line 2)
    (org-edit-src-code)))

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

(require 'rust-mode)
(evil-leader/set-key-for-mode 'rust-mode "TAB" 'rust-format-buffer)

(add-hook 'rust-mode-hook #'cargo-minor-mode)
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)
(add-hook 'rust-mode-hook #'flycheck-mode)
(with-eval-after-load 'rust-mode
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))
(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
(setq company-tooltip-align-annotations t)

(setq sh-basic-offset 2
      sh-indentation 2)

(custom-set-faces
 '(aw-leading-char-face
   ((t (:inherit ace-jump-face-foreground :height 0.95)))))



;; Elixir
(add-hook 'alchemist-iex-mode-hook
          (lambda ()
            (setq show-trailing-whitespace nil)))
(add-hook 'alchemist-test-report-mode-hook
          (lambda ()
            (setq truncate-lines t)))
(add-hook 'elixir-mode-hook
          (lambda () (add-hook 'before-save-hook 'elixir-format nil t)))

(evil-leader/set-key-for-mode 'elixir-mode "eb" 'alchemist-execute-this-buffer)
(evil-leader/set-key-for-mode 'elixir-mode "er" 'alchemist-send-region)
(evil-leader/set-key-for-mode 'elixir-mode "\\" 'alchemist-mix-test-this-buffer)
(evil-leader/set-key-for-mode 'elixir-mode "]" 'alchemist-mix-test-at-point)

(sp-with-modes '(elixir-mode)
  (sp-local-pair "fn" "end"
         :when '(("SPC" "RET"))
         :actions '(insert navigate))
  (sp-local-pair "do" "end"
         :when '(("SPC" "RET"))
         :post-handlers '(sp-ruby-def-post-handler)
         :actions '(insert navigate)))

;; Company Mode
(setq company-tooltip-limit 20)                      ; bigger popup window
(setq company-idle-delay .2)                         ; decrease delay before autocompletion popup shows
(setq company-echo-delay 0)                          ; remove annoying blinking
(setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing

(setq org-export-allow-bind-keywords 't)
(setq org-html-preamble 'nil)
(setq org-html-preamble-format 'nil)

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (auto-complete-mode 0)
  (define-key tide-mode-map (kbd "TAB") #'company-complete-common-or-cycle)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)
(setq typescript-indent-level 2)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)
(add-hook 'typescript-mode-hook #'setup-tide-mode)

;; TSX with Web Mode + Tide
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))
;; enable typescript-tslint checker
(flycheck-add-mode 'typescript-tslint 'web-mode)

(require 'prettier-js)

(setq lsp-enable-snippet nil)
(setq lsp-ui-doc-position 'top)

(use-package lsp-ui)
(use-package company-lsp)

(use-package lsp-mode
  :config (setq lsp-prefer-flymake nil))

;; Scala + sbt with Metals

(use-package scala-mode
  :mode "\\.s\\(cala\\|bt\\)$")

(setq lsp-ui-doc-enable nil)

(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
   ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
   (setq sbt:program-options '("-Dsbt.supershell=false")))

(add-hook 'scala-mode-hook
          (lambda () (flycheck-mode)))

(use-package company-lsp)
