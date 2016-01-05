;; Set Color Scheme
(load-theme 'solarized-light t)

(defun toggle-theme ()
    (interactive)
    (if (eq (frame-parameter (next-frame) 'background-mode)
	    'light)
	(enable-theme 'solarized-dark)
      (enable-theme 'solarized-light)))

;; Typography
(set-face-attribute 'default nil
		    :family "Source Code Pro"
		    :height 150
		    :weight 'normal
		    :width 'normal)

;; Highlight matching parens
(show-paren-mode t)

;; Tabs and Indentation Configuration
(setq tab-width 2
      indent-tabs-mode nil)

(global-linum-mode)

(setq-default show-trailing-whitespace t)
