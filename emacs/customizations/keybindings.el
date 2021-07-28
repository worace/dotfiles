
(defun scratch ()
  (interactive)
  (find-file "~/Dropbox/notes/scratch.org"))


(evil-leader/set-key "-" 'worace-text-scale-decrease)
(evil-leader/set-key "=" 'worace-text-scale-increase)
(defun worace-text-scale-change (increment direction)
  (let ((cur-height (face-attribute 'default :height)))
    (set-face-attribute 'default
                        nil
                        :height
                        (funcall direction cur-height increment))))

(defun worace-text-scale-increase ()
  (interactive)
  (worace-text-scale-change 20 '+))

(defun worace-text-scale-decrease ()
  (interactive)
  (worace-text-scale-change 20 '-))

(if (eq system-type 'gnu/linux)
    (setq  x-meta-keysym 'super
           x-super-keysym 'meta))

(setq mac-command-modifier 'super)
(setq mac-option-modifier 'meta)
(global-set-key (kbd "s--") 'worace-text-scale-decrease)
(global-set-key (kbd "s-=") 'worace-text-scale-increase)
(global-set-key (kbd "s-v") 'clipboard-yank)

(defun seeing-is-believing-evaluate-current-line ()
  (interactive)
  (seeing-is-believing-mark-current-line-for-xmpfilter)
  (seeing-is-believing-run-as-xmpfilter))

;;Elixir
(evil-leader/set-key-for-mode 'alchemist-mode "]" 'alchemist-mix-test-at-point)
(evil-leader/set-key-for-mode 'alchemist-mode "\\" 'alchemist-mix-test-this-buffer)

;; Git commands
(evil-leader/set-key "gs" 'magit-status)
(evil-leader/set-key "gc" 'magit-commit)
(evil-leader/set-key "gp" 'magit-push-popup)
(evil-leader/set-key "gf" 'magit-pull)
(evil-leader/set-key "gb" 'magit-blame-addition)
(evil-leader/set-key "gq" 'magit-blame-quit)
(evil-leader/set-key "go" 'magit-show-commit)

(defun hub-browse ()
  (interactive)
  (shell-command (concat "gh repo view --web")))
(evil-leader/set-key "hb" 'hub-browse)

(defun keybind ()
  (interactive)
  (find-file "~/dotfiles/.emacs.d/customizations/keybindings.el"))

(defun dots ()
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


;; Markdown
(evil-leader/set-key-for-mode 'markdown-mode "o" 'markdown-follow-link-at-point)

(defun edit-files ()
  "Open a a bunch of files, given a text file containing a list of file names"
  (interactive)
  (setq my_filelist (completing-read "my_filelist: " nil nil nil))
  (with-temp-buffer
    (insert-file-contents my_filelist)
    (goto-char (point-min))
    (while (not (eobp))
      (find-file-noselect (replace-regexp-in-string "\n$" "" (thing-at-point 'line t)))
      (forward-line))))

(defun brb ()
  "Open the BRB note file for stream viewers"
  (interactive)
  (find-file "~/Dropbox/scratch/brb.txt"))
