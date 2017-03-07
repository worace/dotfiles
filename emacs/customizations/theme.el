;; Set Color Scheme
;; (load-theme 'solarized-light t)
(load-theme 'gruvbox t)

(defun toggle-theme ()
    (interactive)
    (if (eq (frame-parameter (next-frame) 'background-mode)
	    'light)
        (load-theme 'solarized-dark)
      (load-theme 'solarized-light)))

;; Typography
(set-face-attribute 'default nil
		    :family "Source Code Pro"
		    :height 160
		    :weight 'normal
		    :width 'normal)

;; Highlight matching parens
(show-paren-mode t)

;; Tabs and Indentation Configuration
(setq tab-width 2
      indent-tabs-mode nil)

(global-linum-mode)
(defun linum-update-window-scale-fix (win)
  "fix linum for scaled text"
  (set-window-margins win
		      (ceiling (* (if (boundp 'text-scale-mode-step)
				      (expt text-scale-mode-step
					    text-scale-mode-amount) 1)
				  (if (car (window-margins))
				      (car (window-margins)) 1)
				  ))))
(advice-add 'linum-update-window :after 'linum-update-window-scale-fix)

(setq-default show-trailing-whitespace t)

(defun disable-trailing-whitespace ()
  (interactive)
  (setq show-trailing-whitespace nil))

(defun enable-trailing-whitespace ()
  (interactive)
  (setq show-trailing-whitespace t))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
