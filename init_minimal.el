;;; init_minimal.el --- Minimal Emacs config -*- lexical-binding: t; -*-

;; ============================================================
;; 1. Bootstrap
;; ============================================================

;; Suppress "Package cl is deprecated" warning from older dependencies
(setq byte-compile-warnings '(cl-functions))

;; Raise GC threshold for snappier startup / LSP
(setq gc-cons-threshold 100000000
      read-process-output-max (* 1024 1024)) ;; 1MB for LSP

;; Package setup — use-package is built-in since Emacs 29, but bootstrap
;; for older versions just in case.
(require 'package)
(setq package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; ============================================================
;; 2. General settings
;; ============================================================

(setq-default indent-tabs-mode nil
              tab-width 2
              truncate-lines t)

(defalias 'yes-or-no-p 'y-or-n-p)

;; Backups / autosave — keep them out of the way
(setq backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      create-lockfiles nil
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; Recent files
(require 'recentf)
(setq recentf-max-menu-items 2000)
(recentf-mode 1)
(run-at-time nil (* 5 60) 'recentf-save-list)

;; macOS modifier keys
(setq mac-command-modifier 'super
      mac-option-modifier 'meta)

;; Load secrets if present
(when (file-exists-p "~/.secrets.el")
  (load "~/.secrets.el"))

;; Shell PATH on macOS
(use-package exec-path-from-shell
  :config
  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize))

;; ============================================================
;; 3. UI
;; ============================================================

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

(setq inhibit-splash-screen t
      initial-scratch-message nil
      initial-major-mode 'org-mode)

;; Scrolling — vim-style smooth
(setq scroll-margin 1
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1
      auto-window-vscroll nil)

;; Misc UI
(setq echo-keystrokes 0.1
      use-dialog-box nil
      blink-cursor-mode nil)

;; Visual bell — mode-line flash
(defun worace/mode-line-visual-bell ()
  "A friendlier visual bell effect."
  (invert-face 'mode-line)
  (run-with-timer 0.05 nil 'invert-face 'mode-line))
(setq visible-bell nil
      ring-bell-function 'worace/mode-line-visual-bell)

;; Line numbers
(global-display-line-numbers-mode 1)
(set-fringe-mode 0)

;; Trailing whitespace
(setq-default show-trailing-whitespace t)
;; Disable in non-file buffers where it's just noise
(dolist (hook '(term-mode-hook comint-mode-hook compilation-mode-hook
               minibuffer-setup-hook))
  (add-hook hook (lambda () (setq show-trailing-whitespace nil))))

;; Empty line indicators (vim tilde style)
(setq-default indicate-empty-lines t)

;; Show matching parens
(show-paren-mode t)

;; Escape quits things
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Mac clipboard
(global-set-key (kbd "s-v") 'clipboard-yank)

;; Auto-create parent directories when visiting new files
(defadvice find-file (before make-directory-maybe (filename &optional wildcards) activate)
  "Create parent directory if not exists while visiting file."
  (unless (file-exists-p filename)
    (let ((dir (file-name-directory filename)))
      (unless (file-exists-p dir)
        (make-directory dir t)))))

;; Text mode settings (org, markdown, etc)
(defun worace/text-mode-hook ()
  (turn-on-visual-line-mode)
  (toggle-word-wrap 1)
  (setq evil-cross-lines t))
(add-hook 'text-mode-hook 'worace/text-mode-hook)

;; ============================================================
;; 4. Theme
;; ============================================================

(use-package gruvbox-theme
  :config
  (load-theme 'gruvbox-dark-medium t))

(use-package solarized-theme
  :defer t)

;; Font
(set-face-attribute 'default nil
                    :family "Menlo"
                    :height 120
                    :weight 'normal
                    :width 'normal)

;; Font scaling
(defun worace/text-scale-change (increment direction)
  (let ((cur-height (face-attribute 'default :height)))
    (set-face-attribute 'default nil :height (funcall direction cur-height increment))))

(defun worace/text-scale-increase ()
  (interactive)
  (worace/text-scale-change 20 '+))

(defun worace/text-scale-decrease ()
  (interactive)
  (worace/text-scale-change 20 '-))

(global-set-key (kbd "s--") 'worace/text-scale-decrease)
(global-set-key (kbd "s-=") 'worace/text-scale-increase)

;; ============================================================
;; 5. Evil mode
;; ============================================================

(use-package undo-tree
  :config
  (global-undo-tree-mode)
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))))

(use-package evil-leader
  :config
  (evil-leader/set-leader "<SPC>")
  (global-evil-leader-mode))

(use-package evil
  :after (evil-leader undo-tree)
  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-tree)

  ;; Visual line movement (j/k)
  (define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
  (define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
  (define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)

  ;; Treat symbols as words (e.g. underscores)
  (defalias #'forward-evil-word #'forward-evil-symbol)

  ;; G / gg
  (evil-define-key 'normal global-map "G" 'end-of-buffer)
  (evil-define-key 'normal dired-mode-map "gg" 'beginning-of-buffer)

  ;; Prevent C-w C-h from opening keymap help
  (define-key evil-normal-state-map (kbd "C-w C-h") 'evil-window-left))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1)
  (evil-define-key 'visual evil-surround-mode-map "s" 'evil-surround-edit))

;; ============================================================
;; 6. Leader bindings
;; ============================================================

;; General
(evil-leader/set-key "q" 'evil-quit)
(evil-leader/set-key "k" 'kill-current-buffer)
(evil-leader/set-key "s" 'save-buffer)
(evil-leader/set-key "d" 'dired)
(evil-leader/set-key "/" 'comment-or-uncomment-region)
(evil-leader/set-key "-" 'worace/text-scale-decrease)
(evil-leader/set-key "=" 'worace/text-scale-increase)

;; Theme toggle
(defun worace/toggle-theme ()
  (interactive)
  (if (eq (frame-parameter (next-frame) 'background-mode) 'light)
      (load-theme 'gruvbox-dark-medium t)
    (load-theme 'solarized-light t)))
(evil-leader/set-key "ct" 'worace/toggle-theme)

;; ============================================================
;; 7. Completion — company
;; ============================================================

(use-package company
  :hook (after-init . global-company-mode)
  :config
  (setq company-tooltip-limit 20
        company-idle-delay 0.2
        company-echo-delay 0
        company-begin-commands '(self-insert-command)
        company-tooltip-align-annotations t))

;; ============================================================
;; 8. Navigation — helm, projectile, ace
;; ============================================================

(use-package helm
  :bind (("M-x" . helm-M-x)
         ("C-x b" . helm-buffers-list))
  :init
  (evil-leader/set-key "b" 'helm-buffers-list)
  (evil-leader/set-key "r" 'helm-recentf)
  (evil-leader/set-key "x" 'helm-M-x)
  :config
  (helm-mode 1))

(use-package projectile
  :config
  (projectile-mode 1)
  (setq projectile-git-submodule-command nil))

(use-package helm-projectile
  :after (helm projectile)
  :config
  (helm-projectile-on)
  (evil-leader/set-key "t" 'helm-projectile-find-file-dwim)
  ;; Workaround: helm-ff--in-backup-directory passes nil to file-equal-p
  ;; when called via helm-projectile's action transformer (which doesn't
  ;; set helm-ff-default-directory). This check is cosmetic — it only
  ;; tweaks which actions appear in the menu.
  (advice-add 'helm-ff--in-backup-directory :around
              (lambda (orig-fn &rest args)
                (ignore-errors (apply orig-fn args)))))

(use-package helm-rg
  :after helm
  :config
  (setq helm-rg-default-directory 'git-root)
  (evil-leader/set-key "f" 'helm-projectile-rg)
  (evil-leader/set-key "F" 'helm-resume))

(use-package ace-jump-mode
  :config
  (evil-leader/set-key "<SPC>" 'ace-jump-char-mode))

(use-package ace-window
  :config
  (evil-leader/set-key "w" 'ace-window)
  (evil-leader/set-key "asw" 'ace-swap-window))

;; ============================================================
;; 9. Editing — smartparens, rainbow delimiters
;; ============================================================

(use-package smartparens
  :config
  (smartparens-global-mode t))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; ============================================================
;; 10. Git
;; ============================================================

(use-package magit
  :config
  (setq git-commit-summary-max-length 70))

(evil-leader/set-key "gs" 'magit-status)
(evil-leader/set-key "gc" 'magit-commit)
(evil-leader/set-key "gp" 'magit-push)
(evil-leader/set-key "gf" 'magit-pull)
(evil-leader/set-key "gb" 'magit-blame-addition)
(evil-leader/set-key "gq" 'magit-blame-quit)
(evil-leader/set-key "go" 'magit-show-commit)

;; GitHub CLI integration
(defun worace/hub-browse ()
  (interactive)
  (shell-command "gh repo view --web"))

(defun worace/current-file-repo-relative ()
  (replace-regexp-in-string (regexp-quote (magit-toplevel)) "" (buffer-file-name)))

(defun worace/hub-open ()
  (interactive)
  (let ((default-directory (magit-toplevel)))
    (shell-command (concat "gh browse " (worace/current-file-repo-relative)))))

(defun worace/hub-open-line ()
  (interactive)
  (let ((default-directory (magit-toplevel)))
    (shell-command (concat "gh browse " (worace/current-file-repo-relative) ":" (format-mode-line "%l")))))

(evil-leader/set-key "hb" 'worace/hub-browse)
(evil-leader/set-key "ho" 'worace/hub-open)
(evil-leader/set-key "hl" 'worace/hub-open-line)

;; ============================================================
;; 11. LSP
;; ============================================================

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :config
  (setq lsp-prefer-flymake nil
        lsp-enable-snippet nil
        lsp-headerline-breadcrumb-enable nil)
  ;; Navigation: super-b to go to implementation, leader [ / ] for back/forward
  (define-key lsp-mode-map (kbd "s-b") 'lsp-find-definition)
  (evil-leader/set-key "[" 'xref-go-back)
  (evil-leader/set-key "]" 'xref-go-forward))

(use-package lsp-ui
  :after lsp-mode
  :config
  (setq lsp-ui-doc-enable nil
        lsp-ui-sideline-enable t))

;; ============================================================
;; 12. Python
;; ============================================================

(use-package python
  :ensure nil
  :mode ("\\.py\\'" . python-mode)
  :hook (python-mode . lsp-deferred)
  :config
  (setq python-indent-offset 4
        python-indent-guess-indent-offset nil))

(use-package lsp-pyright
  :after (python lsp-mode)
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred))))

;; ============================================================
;; 13. TypeScript
;; ============================================================

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

(use-package web-mode
  :mode ("\\.tsx\\'" "\\.jsx\\'")
  :hook (web-mode . (lambda ()
                      (when (string-match-p "tsx\\|jsx" (file-name-extension buffer-file-name ""))
                        (lsp-deferred))))
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-enable-auto-closing t
        web-mode-enable-auto-pairing nil))

;; ============================================================
;; 14. Flycheck
;; ============================================================

(use-package flycheck
  :hook (prog-mode . flycheck-mode))

;; ============================================================
;; 15. Extra file modes (lightweight, no config)
;; ============================================================

(use-package yaml-mode)
(use-package json-mode)
(use-package markdown-mode
  :mode ("\\.md\\'" "\\.markdown\\'"))
(use-package dockerfile-mode)

;; ============================================================
;; 16. Org mode (minimal)
;; ============================================================

(use-package org
  :ensure nil
  :mode ("\\.org\\'" . org-mode)
  :hook ((org-mode . org-indent-mode)
         (org-mode . (lambda () (display-line-numbers-mode 0))))
  :config
  (setq org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-src-preserve-indentation nil
        org-edit-src-content-indentation 0
        org-startup-folded t
        org-ellipsis " …")
  (evil-leader/set-key-for-mode 'org-mode "o" 'org-open-at-point)
  (evil-leader/set-key-for-mode 'org-mode "is" 'org-edit-src-code)
  (evil-leader/set-key-for-mode 'org-mode "il" 'org-insert-link)
  (evil-leader/set-key-for-mode 'org-mode "ii" 'org-insert-list-item)
  (evil-leader/set-key-for-mode 'org-mode "[" 'org-promote-subtree)
  (evil-leader/set-key-for-mode 'org-mode "]" 'org-demote-subtree)
  ;; Pretty bullets for list items
  (font-lock-add-keywords 'org-mode
                          '(("^ +\\([-*]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•")))))))

;; ============================================================
;; 17. Utility commands
;; ============================================================

(defun worace/scratch ()
  (interactive)
  (find-file "~/Dropbox/notes/scratch.org"))

(defun worace/dots ()
  (interactive)
  (dired "~/dotfiles"))

(defun worace/edit-init ()
  (interactive)
  (find-file "~/dotfiles/init_minimal.el"))

;; Elisp eval bindings
(evil-leader/set-key-for-mode 'emacs-lisp-mode "eb" 'eval-buffer)
(evil-leader/set-key-for-mode 'emacs-lisp-mode "er" 'eval-region)

;; Copy file reference (repo-relative path + line numbers) to clipboard
(defun worace/file-ref ()
  "Copy repo-relative file path with line number(s) to clipboard.
In normal state: path:LINE. In visual state: path:START-END."
  (interactive)
  (let* ((path (worace/current-file-repo-relative))
         (ref (if (evil-visual-state-p)
                  (let ((start (line-number-at-pos (region-beginning)))
                        (end (line-number-at-pos (region-end))))
                    (when (and (= (region-end) (line-beginning-position))
                               (> end start))
                      (setq end (1- end)))
                    (format "%s:%d-%d" path start end))
                (format "%s:%d" path (line-number-at-pos)))))
    (kill-new ref)
    (message "%s" ref)))
(evil-leader/set-key "l" 'worace/file-ref)

;; Markdown follow-link
(evil-leader/set-key-for-mode 'markdown-mode "o" 'markdown-follow-link-at-point)

;;; init_minimal.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(ace-jump-mode ace-window company dockerfile-mode evil-leader
                   evil-surround exec-path-from-shell flycheck
                   gruvbox-theme helm-projectile helm-rg json-mode
                   lsp-pyright lsp-ui magit rainbow-delimiters
                   smartparens solarized-theme typescript-mode
                   undo-tree web-mode yaml-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
