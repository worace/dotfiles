;; (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
;; (setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
;; 			 ("melpa" . "https://melpa.org/packages/")
;; 			 ("melpa-stable" . "https://stable.melpa.org/packages/")))

(use-package dash)

;; Define list of packages to install
(defvar worace/packages '(dash
                          dash-functional
                          evil
                          evil-leader
                          evil-surround
                          ))

(when (boundp 'package-pinned-packages)
  (setq package-pinned-packages
	'((cider              . "melpa-stable")
    (yaml-mode          . "melpa-stable")
    (helm-config          . "melpa-stable")
    (clj-refactor       . "melpa-stable"))))
