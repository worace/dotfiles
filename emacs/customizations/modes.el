(require 'use-package)

(if (file-exists-p "~/.secrets.el")
    (load "~/.secrets.el"))

(setq-default truncate-lines 1)
(setq-default indent-tabs-mode nil)
(global-linum-mode 1)
(ido-mode 1)
(global-auto-complete-mode)
(setq company-tooltip-align-annotations t)
(setq sh-basic-offset 2
      sh-indentation 2)
;;Hub Github Addon
;;Currently just installed locally
(require 'hub)

;; Save Recent Files
(require 'recentf)
(setq recentf-max-menu-items 2000)
(recentf-mode 1)
(run-at-time nil (* 5 60) 'recentf-save-list)

;; elisp
(add-to-list 'auto-mode-alist '("\\.el\\'" . emacs-lisp-mode))
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)

;; Postgres / SQL Mode
;; sql-connection-alist defined in ~/.secrets.el
(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (toggle-truncate-lines t)
            (disable-trailing-whitespace)))

(add-to-list 'auto-mode-alist '("\\.txt\\'" . text-mode))
;; Text Mode (Org, Markdown, etc)
(defun worace-text-mode-hook ()
  (turn-on-visual-line-mode)
  (toggle-word-wrap 1)
  (setq evil-cross-lines t))
(add-hook 'text-mode-hook 'worace-text-mode-hook)

(use-package ace-jump-mode :ensure t
  :config
  (custom-set-faces
   '(aw-leading-char-face
     ((t (:inherit ace-jump-face-foreground :height 0.95))))))
(use-package ace-window :ensure t)

(use-package helm
  :ensure t
  :bind (("M-x" . helm-M-x)
         ("C-x b" . helm-buffers-list))
  :config (helm-mode 1))

(use-package projectile
  :ensure t
  :config
  (setq projectile-git-submodule-command nil))


(use-package rainbow-delimiters
  :ensure t)

(use-package clojure-mode
  :ensure t
  :hook ((clojure-mode . #'rainbow-delimiters-mode)
         (clojure-mode . #'smartparens-strict-mode))
  :config
  (setq cljr-inject-dependencies-at-jack-in nil)
  (setq cider-show-error-buffer nil)
  (setq cider-use-fringe-indicators nil)
  (setq cider-repl-history-size 1000)
  (setq cider-repl-history-file "~/.ciderhistory"))

(use-package cider
  :ensure t
  :hook (cider-repl-mode . (lambda ()
                             (setq show-trailing-whitespace nil)
                             (setq truncate-lines nil))))

(use-package smartparens
  :ensure t
  :config
  (smartparens-global-mode t))

(use-package markdown-mode
  :ensure t
  :mode (".text$" ".markdown$" ".md$")
  :config
  (sp-local-pair 'markdown-mode "`" nil :actions '(insert))
  (custom-set-variables
   '(markdown-command (executable-find "markdown"))))

(use-package js2-mode
  :hook ((js2-mode . (lambda ()
                       (toggle-truncate-lines nil)
                       (flycheck-mode t)
                       (when (executable-find "eslint")
                         (flycheck-select-checker 'javascript-eslint)
                         (use-eslint-from-node-modules)))))
  :config
  (setq js2-bounce-indent-p t
        js2-include-node-externs t
        js2-include-browser-externs t
        js2-basic-offset 2
        js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil
        js2-strict-trailing-comma-warning nil
        js2-strict-missing-semi-warning nil
        js2-strict-inconsistent-return-warning nil
        jsx-indent-level 2
        js-indent-level 2)
  (setq-default js2-global-externs
                '("module" "require" "sinon" "assert" "refute" "setTimeout"
                  "clearTimeout" "setInterval" "clearInterval" "location"
                  "__dirname" "console" "JSON" "describe" "it" "beforeEach"
                  "before" "after" "afterEach")))

;; (use-package js2-jsx-mode
;;   :ensure t
;;   :mode (".jsx$" ".js$"))

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

(use-package magit
  :ensure t
  :config
  (setq git-commit-summary-max-length 70))

(use-package chruby
  :ensure t
  :config
  (chruby "2.4.4")
  )
(use-package seeing-is-believing
  :ensure t
  :hook ((ruby-mode . #'seeing-is-believing))
  )

(use-package inf-ruby
  :ensure t
  :hook ((ruby-mode . #'inf-ruby-minor-mode))
  :config
  (when (executable-find "pry")
    (add-to-list 'inf-ruby-implementations '("pry" . "pry"))
    (setq inf-ruby-default-implementation "pry")))

(use-package ruby-mode
  :hook ((ruby-mode . (lambda ()
                        (add-hook 'compilation-finish-functions 'my-compilation-finish-hook nil 'make-it-local)
                        )))
  :config
  (setq ruby-deep-arglist nil)
  (setq ruby-deep-indent-paren nil))

(defun my-compilation-finish-hook (buf strg)
  (switch-to-buffer-other-window "*compilation*")
  (read-only-mode)
  (goto-char (point-max))
  (local-set-key (kbd "q")
                 (lambda () (interactive) (quit-restore-window))))

(use-package ruby-test-mode
  :ensure t
  :hook ((ruby-mode . #'ruby-test-mode)))

;;;;;;;;;;;;;
;; Python ;;;
;;;;;;;;;;;;;

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :hook ((python-mode . #'flycheck-mode)
         (inferior-python-mode . (lambda ()
                                   (disable-trailing-whitespace)
                                   (local-set-key (kbd "C-c C-k")
                                                  'python-shell-clear-output))))
  :config
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "--simple-prompt -i"
        py-shell-name "ipython"
        py-which-bufname "IPython"
        py-shell-switch-buffers-on-execute-p t
        py-switch-buffers-on-execute-p t
        py-smart-indentation t
        )
  (custom-set-variables
   '(python-guess-indent nil)
   '(python-indent 2)))

(defun python-shell-clear-output ()
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

(use-package virtualenvwrapper
  :ensure t
  :config
  (setq venv-location "~/.virtualenvs"))

(use-package flycheck
  :ensure t)

(use-package web-mode
  :ensure t
  :mode (".erb$" ".scss$" ".css$" ".html?$" ".hbs$" ".eex$" ".tsx$")
  :config (setq web-mode-markup-indent-offset 2
                web-mode-enable-auto-pairing nil
                web-mode-enable-auto-closing t
                web-mode-css-indent-offset 2
                web-mode-code-indent-offset 2))

;; Support ERB/EEX tags in SmartParens
;; https://emacs.stackexchange.com/questions/15188/smartparens-and-web-mode-conflict-to-add-extra-angular-bracket
;; (sp-pair "%" "%" :wrap "C-%")
;; (sp-pair "<" ">" :wrap "C->")


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
         (org-mode . (lambda () (linum-mode 0)))
         (org-mode . (lambda () (org-indent-mode 1)))
         (org-mode . org-bullets-mode))
  :config
  (setq org-src-fontify-natively t
        inhibit-compacting-font-caches t
        org-startup-folded t
        org-src-tab-acts-natively t
        org-src-preserve-indentation nil
        org-edit-src-content-indentation 0
        org-export-allow-bind-keywords t
        org-html-preamble nil
        org-html-preamble-format nil)
  ;; use pretty unicode bullets for lists
  (font-lock-add-keywords 'org-mode
                          '(("^ +\\([-*]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•")))))))

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

(use-package flycheck-rust
  :ensure t)
(use-package rust-mode
  :ensure t
  :hook ((rust-mode . #'cargo-minor-mode)
         (rust-mode . #'racer-mode)
         (rust-mode . #'flycheck-mode)
         (flycheck-mode . #'flycheck-rust-setup))
  :config
  (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common))

(use-package racer
  :ensure t
  :hook ((racer-mode . #'eldoc-mode)
         (racer-mode . #'company-mode)))


(use-package alchemist
  :ensure t
  :hook ((alchemist-iex-mode . show-trailing-whitespace)
         (alchemist-test-report-mode . truncate-lines)
         (before-save . 'elixir-format)))

(use-package elixir-mode
  :ensure t
  :config
  (sp-with-modes '(elixir-mode)
    (sp-local-pair "fn" "end"
                   :when '(("SPC" "RET"))
                   :actions '(insert navigate))
    (sp-local-pair "do" "end"
                   :when '(("SPC" "RET"))
                   :post-handlers '(sp-ruby-def-post-handler)
                   :actions '(insert navigate))))

(use-package company
  :config
  (setq company-tooltip-limit 20)                      ; bigger popup window
  (setq company-idle-delay .2)                         ; decrease delay before autocompletion popup shows
  (setq company-echo-delay 0)                          ; remove annoying blinking
  (setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
  (setq company-tooltip-align-annotations t)
  )

(use-package typescript-mode
  :ensure t
  :config
  (setq typescript-indent-level 2)
  (flycheck-add-mode 'typescript-tslint 'web-mode))

(use-package tide
  :ensure t
  :hook ((typescript-mode . #'setup-tide-mode)
         (before-save . #'tide-format-before-save)
         (web-mode . #'tide-web-mode-setup)))

(defun setup-tide-mode ()
  (tide-setup)
  (flycheck-mode 1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode 1)
  (tide-hl-identifier-mode 1)
  (auto-complete-mode 0)
  (define-key tide-mode-map (kbd "TAB") #'company-complete-common-or-cycle)
  (company-mode 1))

(defun tide-web-mode-setup ()
  (when (string-equal "tsx" (file-name-extension buffer-file-name))
    (setup-tide-mode)))

(use-package yaml-mode :ensure t)
(use-package prettier-js
  :ensure t)

(use-package lsp-ui :ensure t
  :config
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-doc-position 'top))

(use-package company-lsp :ensure t)

(use-package lsp-mode
  :ensure t
  :config
  (setq lsp-prefer-flymake nil)
  (setq lsp-enable-snippet nil))

;; Scala + sbt with Metals
(use-package scala-mode
  :ensure t
  :mode "\\.s\\(cala\\|bt\\)$")

(use-package sbt-mode
  :ensure t
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

(use-package company-lsp
  :ensure t)

;;;;;;;;;;;
;; Evil stuff

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

;; Evil Bindings
(evil-leader/set-key-for-mode 'elixir-mode "eb" 'alchemist-execute-this-buffer)
(evil-leader/set-key-for-mode 'elixir-mode "er" 'alchemist-send-region)
(evil-leader/set-key-for-mode 'elixir-mode "\\" 'alchemist-mix-test-this-buffer)
(evil-leader/set-key-for-mode 'elixir-mode "]" 'alchemist-mix-test-at-point)
(define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
;; Was losing evil gg in dired mode for some reason
;; probably using dired wrong but this fixes it...
(evil-define-key 'normal dired-mode-map "gg" 'beginning-of-buffer)
(global-evil-surround-mode 1)

;;Evil Bindings
(evil-leader/set-key-for-mode 'org-mode "o" 'org-open-at-point)
(evil-leader/set-key-for-mode 'org-mode "i s" 'org-edit-src-code)
(evil-leader/set-key-for-mode 'org-mode "i l" 'org-insert-link)
(evil-leader/set-key-for-mode 'org-mode "i i" 'org-insert-list-item)

(evil-leader/set-key-for-mode 'rust-mode "TAB" 'rust-format-buffer)
