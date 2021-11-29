(if (file-exists-p "~/.secrets.el")
    (load "~/.secrets.el"))

;; (setq max-lisp-eval-depth 1000)
;; (setq max-specpdl-size 32000)
(setq-default truncate-lines 1)
(setq-default indent-tabs-mode nil)
(global-linum-mode 1)
(ido-mode 1)
(setq company-tooltip-align-annotations t)
(setq sh-basic-offset 2
      sh-indentation 2)

;; Save Recent Files
(require 'recentf)
(setq recentf-max-menu-items 2000)
(recentf-mode 1)
(run-at-time nil (* 5 60) 'recentf-save-list)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Elisp
(use-package emacs-lisp
  :ensure nil
  :mode (".el$" . emacs-lisp-mode)
  :config
  (evil-leader/set-key-for-mode 'emacs-lisp-mode "eb" 'eval-buffer)
  (evil-leader/set-key-for-mode 'emacs-lisp-mode "er" 'eval-region)
  (setq max-lisp-eval-depth 10000)
  (setq max-specpdl-size 32000))

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

(use-package ace-jump-mode
  :config
  (custom-set-faces
   '(aw-leading-char-face
     ((t (:inherit ace-jump-face-foreground :height 0.95)))))
  (evil-leader/set-key "<SPC>" 'ace-jump-char-mode))

(use-package ace-window
  :config
  (evil-leader/set-key "w" 'ace-window)
  (evil-leader/set-key "asw" 'ace-swap-window))

(use-package auto-complete
  :config
  (global-auto-complete-mode))

(use-package helm
  :bind (("M-x" . helm-M-x)
         ("C-x b" . helm-buffers-list))
  :init
  (helm-mode 1)
  (evil-leader/set-key "b" 'helm-buffers-list)
  (evil-leader/set-key "t" 'helm-projectile-find-file-dwim)
  (evil-leader/set-key "r" 'helm-recentf)
  (evil-leader/set-key "x" 'helm-M-x))

(use-package projectile
  :config
  (setq projectile-git-submodule-command nil))

(use-package smartparens
  :config
  (smartparens-global-mode t))

(use-package markdown-mode
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
  :config
  (setq git-commit-summary-max-length 70))

(use-package chruby
  :config
  (chruby "2.4.4"))

(use-package seeing-is-believing
  :hook ((ruby-mode . #'seeing-is-believing))
  :config
  (evil-leader/set-key-for-mode 'ruby-mode "eb" 'seeing-is-believing-run)
  (evil-leader/set-key-for-mode 'ruby-mode "ec" 'seeing-is-believing-clear)
  (evil-leader/set-key-for-mode 'ruby-mode "er" 'seeing-is-believing-run-as-xmpfilter)
  (evil-leader/set-key-for-mode 'ruby-mode "et" 'seeing-is-believing-mark-current-line-for-xmpfilter)
  (evil-leader/set-key-for-mode 'ruby-mode "el" 'seeing-is-believing-evaluate-current-line))

(use-package inf-ruby
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
  :hook ((ruby-mode . #'ruby-test-mode))
  :config
  (evil-leader/set-key-for-mode 'ruby-mode "\\" 'ruby-test-run)
  (evil-leader/set-key-for-mode 'ruby-mode "]" 'ruby-test-run-at-point))

;;;;;;;;;;;;;
;; Python ;;;
;;;;;;;;;;;;;

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
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
   '(python-indent 4)))

(defun python-shell-clear-output ()
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

(use-package virtualenvwrapper
  :config
  (setq venv-location "~/.virtualenvs"))

(use-package flycheck)

(defun worace-org-mode-setup ()
  ;; keybinding for inserting code blocks
  (local-set-key (kbd "C-c s i") 'org-insert-src-block)
  (local-set-key (kbd "C-c s e") 'org-insert-example-block)
  (let* ((variable-tuple (cond ((x-family-fonts "Sans Serif") '(:family "Sans Serif"))
                               (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
         (base-font-color     (face-foreground 'default nil 'default))
         (headline           `(:inherit default :foreground ,base-font-color)))))

(use-package org
  :mode ("\\.org\\'" . org-mode)
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
  (evil-leader/set-key-for-mode 'org-mode "o" 'org-open-at-point)
  (evil-leader/set-key-for-mode 'org-mode "i s" 'org-edit-src-code)
  (evil-leader/set-key-for-mode 'org-mode "i l" 'org-insert-link)
  (evil-leader/set-key-for-mode 'org-mode "i i" 'org-insert-list-item)
  (evil-leader/set-key-for-mode 'org-mode "[" 'org-promote-subtree)
  (evil-leader/set-key-for-mode 'org-mode "]" 'org-demote-subtree)
  ;; use pretty unicode bullets for lists
  (font-lock-add-keywords 'org-mode
                          '(("^ +\\([-*]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•")))))))

(use-package org-bullets
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

;; https://robert.kra.hn/posts/2021-02-07_rust-with-emacs/
(use-package rustic
  ;; :bind (:map rustic-mode-map
  ;;             ("M-j" . lsp-ui-imenu)
  ;;             ("M-?" . lsp-find-references)
  ;;             ("C-c C-c l" . flycheck-list-errors)
  ;;             ("C-c C-c a" . lsp-execute-code-action)
  ;;             ("C-c C-c r" . lsp-rename)
  ;;             ("C-c C-c q" . lsp-workspace-restart)
  ;;             ("C-c C-c Q" . lsp-workspace-shutdown)
  ;;             ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  ;; uncomment for less flashiness
  (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm
  (setq-local buffer-save-without-query t))

(use-package alchemist
  :hook ((alchemist-iex-mode . show-trailing-whitespace)
         (alchemist-test-report-mode . truncate-lines)
         (before-save . 'elixir-format))
  :config
  (evil-leader/set-key-for-mode 'elixir-mode "eb" 'alchemist-execute-this-buffer)
  (evil-leader/set-key-for-mode 'elixir-mode "er" 'alchemist-send-region)
  (evil-leader/set-key-for-mode 'elixir-mode "\\" 'alchemist-mix-test-this-buffer)
  (evil-leader/set-key-for-mode 'elixir-mode "]" 'alchemist-mix-test-at-point))

(use-package elixir-mode
  :config
  (sp-with-modes '(elixir-mode)
    (sp-local-pair "fn" "end"
                   :when '(("SPC" "RET"))
                   :actions '(insert navigate))
    (sp-local-pair "do" "end"
                   :when '(("SPC" "RET"))
                   :post-handlers '(sp-ruby-def-post-handler)
                   :actions '(insert navigate))))

(use-package web-mode
  :mode ("\\.erb$" "\\.scss$" "\\.css$" "\\.html?$" "\\.hbs$" "\\.eex$" "\\.tsx$" "\\.jsx$")
  :config (setq web-mode-markup-indent-offset 2
                web-mode-enable-auto-pairing nil
                web-mode-enable-auto-closing t
                web-mode-css-indent-offset 2
                web-mode-code-indent-offset 2))

(defun tide-web-mode-setup ()
  (message "Run tide web mode")
  (when (string-equal "tsx" (file-name-extension buffer-file-name))
    (setup-tide-mode)))

;; ;; Support ERB/EEX tags in SmartParens
;; ;; https://emacs.stackexchange.com/questions/15188/smartparens-and-web-mode-conflict-to-add-extra-angular-bracket
;; ;; (sp-pair "%" "%" :wrap "C-%")
;; ;; (sp-pair "<" ">" :wrap "C->")

(use-package typescript-mode
  :hook ((web-mode-hook . (lambda ()
                            (flycheck-add-mode 'typescript-tslint 'web-mode))))
  :mode ".ts$"
  :config
  (setq typescript-indent-level 2))

;; (use-package tide
;;   :mode (("\\.ts\\'" . tide-mode))
;;   :diminish tide-mode
;;   :after (typescript-mode company flycheck)
;;   :hook ((typescript-mode . #'setup-tide-mode)
;;          (before-save . #'tide-format-before-save)
;;          (web-mode . tide-web-mode-setup)))

(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (web-mode . tide-web-mode-setup)
         (before-save . tide-format-before-save)))

(defun setup-tide-mode ()
  (interactive)
  (message "setting up tide mode")
  (tide-setup)
  ;; (flycheck-mode 1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode 1)
  (tide-hl-identifier-mode 1)
  (auto-complete-mode 0)
  (define-key tide-mode-map (kbd "TAB") #'company-complete-common-or-cycle)
  (company-mode +1))

(use-package prettier-js)

(use-package cargo)
(use-package dockerfile-mode)
(use-package graphql-mode)
(use-package groovy-mode)
(use-package helm-projectile
  :config
  (evil-leader/set-key "f" 'helm-projectile-rg))
(use-package helm-rg
  :config
  (setq helm-rg-ripgrep-executable "/usr/bin/rg")
  (setq helm-rg-default-directory 'git-root))
(use-package json-reformat)
(use-package markdown-toc)
(use-package json-mode)
(use-package protobuf-mode)
(use-package rainbow-mode)
(use-package restclient)
(use-package rg)
(use-package solarized-theme)
(use-package spotify
  :config
  (evil-leader/set-key "mp" 'spotify-playpause)
  (evil-leader/set-key "mb" 'spotify-previous)
  (evil-leader/set-key "mf" 'spotify-next))
(use-package toml-mode)
(use-package thrift)
(use-package yaml-mode)
(use-package exec-path-from-shell
  :config
  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize))
(use-package dash)
(use-package dash-functional)


;; Scala / Metals / LSP

(use-package lsp-ui
  :config
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-doc-position 'top))

(use-package lsp-mode
  :config
  (setq lsp-prefer-flymake nil)
  (setq lsp-enable-snippet nil))

(use-package scala-mode
  :mode "\\.s\\(cala\\|bt\\)$"
  :interpreter ("scala" . scala-mode))

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

(use-package lsp-metals)
(use-package company
  :config
  (setq company-tooltip-limit 20)                      ; bigger popup window
  (setq company-idle-delay .2)                         ; decrease delay before autocompletion popup shows
  (setq company-echo-delay 0)                          ; remove annoying blinking
  (setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
  (setq company-tooltip-align-annotations t)
  :hook (company-mode . (lambda () (message (auto-complete-mode 0)))))
(use-package play-routes-mode)
