(evil-define-key 'normal global-map "G" 'end-of-buffer)
(evil-leader/set-key "<SPC>" 'ace-jump-char-mode)
(evil-leader/set-key "b" 'previous-buffer)
(evil-leader/set-key "t" 'helm-projectile-find-file-dwim)
(evil-leader/set-key "r" 'helm-recentf)
(evil-leader/set-key "x" 'helm-M-x)
(evil-leader/set-key "s" 'save-buffer)
(evil-leader/set-key "/" 'comment-or-uncomment-region)
(evil-leader/set-key "-" 'text-scale-decrease)
(evil-leader/set-key "=" 'text-scale-increase)
(evil-leader/set-key "w" 'ace-window)
(evil-leader/set-key "ct" 'toggle-theme)

(setq mac-command-modifier 'super)
(global-set-key (kbd "s--") 'text-scale-decrease)
(global-set-key (kbd "s-=") 'text-scale-increase)

;;Clojure
(evil-leader/set-key "eb" 'cider-eval-buffer)
(evil-leader/set-key "er" 'cider-eval-region)

;; Git commands
(evil-leader/set-key "gs" 'magit-status)
(evil-leader/set-key "gc" 'magit-commit)
(evil-leader/set-key "gp" 'magit-push)
(evil-leader/set-key "gf" 'magit-pull)
(evil-leader/set-key "hb" 'hub-browse)

(defun keybind ()
  (interactive)
  (find-file "~/dotfiles/.emacs.d/customizations/keybindings.el"))

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

;; (evil-define-key 'normal global-map (kbd "S-k") 'move-line-up)
;; (define-key evil-normal-state-map (kbd "S-J") 'move-line-down)
;; (define-key evil-normal-state-map (kbd "S-j") 'move-line-down)
