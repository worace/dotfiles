(evil-leader/set-key "<SPC>" 'ace-jump-char-mode)
(evil-leader/set-key "t" 'helm-projectile)
(evil-leader/set-key "r" 'helm-recentf)
(evil-leader/set-key "x" 'helm-M-x)
(evil-leader/set-key "s" 'save-buffer)
(evil-leader/set-key "/" 'comment-or-uncomment-region)
(evil-leader/set-key "-" 'text-scale-decrease)
(evil-leader/set-key "=" 'text-scale-increase)


;; Git commands
(evil-leader/set-key "gs" 'magit-status)
;; TODO -- try to start working with built-in magit keys
;; through the special magit buffer
;; In future possibly add alias function for "stage and commit current file"
