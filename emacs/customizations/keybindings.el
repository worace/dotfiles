(evil-define-key 'normal global-map "G" 'end-of-buffer)
(evil-leader/set-key "<SPC>" 'ace-jump-char-mode)
(evil-leader/set-key "b" 'helm-buffers-list)
(evil-leader/set-key "t" 'helm-projectile-find-file-dwim)
(evil-leader/set-key "r" 'helm-recentf)
(evil-leader/set-key "x" 'helm-M-x)
(evil-leader/set-key "s" 'save-buffer)
(evil-leader/set-key "/" 'comment-or-uncomment-region)
(evil-leader/set-key "-" 'text-scale-decrease)
(evil-leader/set-key "=" 'text-scale-increase)
(evil-leader/set-key "w" 'ace-window)
(evil-leader/set-key "ct" 'toggle-theme)
(evil-leader/set-key "f" 'helm-projectile-ag)
(evil-leader/set-key "q" 'evil-quit)
(evil-leader/set-key "k" 'kill-this-buffer)
(evil-leader/set-key "mp" 'spotify-playpause)
(evil-leader/set-key "mb" 'spotify-previous)
(evil-leader/set-key "mf" 'spotify-next)

;; try to stop me from constantly opening the stupid evil mode keymap help window...
(define-key evil-normal-state-map (kbd "C-w C-h") 'evil-window-left)

;; TODO Dired Bindings:
;; dired-kill-subdir
;; Configure Dired list output:
;; (setq dired-listing-switches "...." --group-directories-first)
;; Don't prompt before copying/deleting dir recursively:
;; (setq dired-recursive-copies 'always)
;; (setq dired-recursive-deletes 'always)
;; Move to trash rather than rm -rf:
;; (setq delete-by-moving-to-trash t)
;; Dired-Jump: C-x C-j -- open dired in directory of current file
;; Investigate:
;; https://github.com/Fuco1/dired-hacks

(evil-leader/set-key "zf" 'origami-toggle-node)
(evil-leader/set-key "za" 'origami-close-all-nodes)
(evil-leader/set-key "zo" 'origami-open-all-nodes)

(evil-define-key 'normal origami-mode-map (kbd "zo") 'origami-toggle-node)

(setq mac-command-modifier 'super)
(global-set-key (kbd "s--") 'text-scale-decrease)
(global-set-key (kbd "s-=") 'text-scale-increase)

(defun seeing-is-believing-evaluate-current-line ()
  (interactive)
  (seeing-is-believing-mark-current-line-for-xmpfilter)
  (seeing-is-believing-run-as-xmpfilter))

;; Ruby
(evil-leader/set-key-for-mode 'ruby-mode "eb" 'seeing-is-believing-run)
(evil-leader/set-key-for-mode 'ruby-mode "ec" 'seeing-is-believing-clear)
(evil-leader/set-key-for-mode 'ruby-mode "er" 'seeing-is-believing-run-as-xmpfilter)
(evil-leader/set-key-for-mode 'ruby-mode "et" 'seeing-is-believing-mark-current-line-for-xmpfilter)
(evil-leader/set-key-for-mode 'ruby-mode "el" 'seeing-is-believing-evaluate-current-line)
(evil-leader/set-key-for-mode 'ruby-mode "\\" 'ruby-test-run)
(evil-leader/set-key-for-mode 'ruby-mode "]" 'ruby-test-run-at-point)

;; Javascript
(evil-leader/set-key-for-mode 'js2-mode "\\" 'mocha-test-file)
(evil-leader/set-key-for-mode 'js2-mode "]" 'mocha-test-at-point)

;; Python
(evil-leader/set-key-for-mode 'python-mode "\\" 'nosetests-module)

;;Clojure
(evil-leader/set-key-for-mode 'clojure-mode "eb" 'cider-eval-buffer)
(evil-leader/set-key-for-mode 'clojure-mode "er" 'cider-eval-region)
(evil-leader/set-key-for-mode 'clojure-mode "ps" 'sp-forward-slurp-sexp)
(evil-leader/set-key-for-mode 'clojure-mode "pb" 'sp-forward-barf-sexp)
(evil-leader/set-key-for-mode 'cider-repl-mode "k" 'cider-repl-clear-buffer)

(evil-leader/set-key-for-mode 'clojurescript-mode "ps" 'sp-forward-slurp-sexp)
(evil-leader/set-key-for-mode 'clojurescript-mode "pb" 'sp-forward-barf-sexp)

;;Emacs Lisp
(evil-leader/set-key-for-mode 'emacs-lisp-mode "eb" 'eval-buffer)

;; Git commands
(evil-leader/set-key "gs" 'magit-status)
(evil-leader/set-key "gc" 'magit-commit)
(evil-leader/set-key "gp" 'magit-push-popup)
(evil-leader/set-key "gf" 'magit-pull)
(evil-leader/set-key "gb" 'magit-blame)
(evil-leader/set-key "gq" 'magit-blame-quit)
(evil-leader/set-key "go" 'magit-show-commit)
(evil-leader/set-key "hb" 'hub-browse)

(defun keybind ()
  (interactive)
  (find-file "~/dotfiles/.emacs.d/customizations/keybindings.el"))

(defun ed-config ()
  (interactive)
  (dired "~/dotfiles"))

(defun edit-today ()
  "opens the outline for today's date in the turing outlines directory"
  (interactive)
  (cd "~/Turing/today")
  (magit-pull "origin" "master")
  (let ((filename (format-time-string "~/Turing/today/source/outlines/%Y-%m-%d.markdown")))
    (find-file filename)))

(evil-define-key 'visual evil-surround-mode-map "s" 'evil-surround-edit)

(defun move-line (n)
  "Move the current line up or down by N lines."
  (interactive "p")
  (setq col (current-column))
  (beginning-of-line) (setq start (point))
  (end-of-line) (forward-char) (setq end (point))
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (insert line-text)
    ;; restore point to original column in moved line
    (forward-line -1)
    (forward-char col)))

(defun move-line-up (n)
  "Move the current line up by N lines."
  (interactive "p")
  (move-line (if (null n) -1 (- n))))

(defun move-line-down (n)
  "Move the current line down by N lines."
  (interactive "p")
  (move-line (if (null n) 1 n)))

(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME.
   Borrowed from Steve Yegge's .emacs: https://sites.google.com/site/steveyegge2/my-dot-emacs-file"
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

(defun take-note (file-name)
  (interactive "sFile Name:")
  (let ((path (concat "~/Dropbox/notes/" file-name)))
    (find-file path)))

;; (evil-define-key 'normal origami-mode-map (kbd "zo") 'move-line-up)

;; (define-key evil-normal-state-map (kbd "S-J") 'move-line-down)
;; (define-key evil-normal-state-map (kbd "S-j") 'move-line-down)
