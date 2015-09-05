;; Set Color Scheme
(load-theme 'solarized-light t)

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
