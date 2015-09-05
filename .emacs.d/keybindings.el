(evil-leader/set-key "<SPC>" 'ace-jump-char-mode)
(evil-leader/set-key "t" 'helm-projectile)
(evil-leader/set-key "r" 'helm-recentf)
(evil-leader/set-key "x" 'helm-M-x)
(evil-leader/set-key "s" 'save-buffer)
(evil-leader/set-key "/" 'comment-or-uncomment-region)
(evil-leader/set-key "-" 'text-scale-decrease)
(evil-leader/set-key "=" 'text-scale-increase)


;; Git commands
(evil-leader/set-key "gs" 'magit-stage-modified)
(evil-leader/set-key "gc" 'magit-commit)
(evil-leader/set-key "gd" 'magit-diff)
