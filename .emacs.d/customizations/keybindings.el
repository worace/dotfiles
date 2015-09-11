(evil-leader/set-key "<SPC>" 'ace-jump-char-mode)
(evil-leader/set-key "t" 'helm-projectile-find-file-dwim)
(evil-leader/set-key "r" 'helm-recentf)
(evil-leader/set-key "x" 'helm-M-x)
(evil-leader/set-key "s" 'save-buffer)
(evil-leader/set-key "/" 'comment-or-uncomment-region)
(evil-leader/set-key "-" 'text-scale-decrease)
(evil-leader/set-key "=" 'text-scale-increase)

;;Clojure
(evil-leader/set-key "b" 'cider-eval-buffer)


;; Git commands
(evil-leader/set-key "gs" 'magit-status)
(evil-leader/set-key "gc" 'magit-commit)
(evil-leader/set-key "gp" 'magit-push)
;; TODO -- try to start working with built-in magit keys
;; through the special magit buffer
;; In future possibly add alias function for "stage and commit current file"


(defun edit-today ()
  "opens the outline for today's date in the turing outlines directory"
  (interactive)
  (cd "~/code/Turing/today")
  (magit-pull "origin" "master")
  (let ((filename (format-time-string "~/code/Turing/today/source/outlines/%Y-%m-%d.markdown")))
    (find-file filename)))


(evil-define-key 'visual evil-surround-mode-map "s" 'evil-surround-edit)
