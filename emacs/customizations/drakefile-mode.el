;;; drakefile-mode.el --- Major mode for editing Drake workflow files

;; Copyright (C) 2017 Horace Williams

;; Author: Horace Williams <horace@worace.works>
;; Version: 0.1
;; Keywords: drake, drakefile
;; URL: http://github.com/worace/drakefile-mode

;;; Commentary:

;; Major mode for editing Drake workflow files. So far just basic
;; syntax highlighting but maybe more features in the future.
;; https://github.com/Factual/drake

(defvar drakefile-highlights nil "first element for `font-lock-defaults'")

(setq drakefile-constant-assignment "\s?\\([a-zA-Z_]+\\)\s?:?=")
(setq drakefile-tags "\\(%[[:alnum:]_\\-]+\\)")
(setq drakefile-variable-injection-brackets "[]$\\[]")
(setq drakefile-variable-injection "\\[\\([[:alnum:]_]+\\)\\]")
(setq drakefile-keywords "\\(<\\-\\)")
(setq drakefile-comment ";.*")

(setq drakefile-font-lock-keywords
      `((,drakefile-comment . font-lock-comment-face)
        (,drakefile-variable-injection . (1 font-lock-variable-name-face))
        (,drakefile-variable-injection-brackets . font-lock-keyword-face)
        (,drakefile-keywords . font-lock-keyword-face)
        (,drakefile-constant-assignment . (1 font-lock-constant-face))
        (,drakefile-tags . font-lock-function-name-face)))

(defconst drakefile-syntax-table
  (let ((table (make-syntax-table)))
    ;; single and double quotes are string delimiters
    (modify-syntax-entry ?' "\"" table)
    (modify-syntax-entry ?\" "\"" table)

    ;; ; is a comment starter
    (modify-syntax-entry ?\; "<" table)
    ;; \n is a comment ender
    (modify-syntax-entry ?\n ">" table)
    ;; Braces
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)

    table))

(define-derived-mode drakefile-mode shell-script-mode "drakefile-mode"
  "Major mode for working with Drakefiles: https://github.com/Factual/drake."
  ;; :syntax-table drakefile-syntax-table
  (toggle-truncate-lines nil)
  (setq-local font-lock-defaults '((drakefile-font-lock-keywords)))
  (setq-local comment-start ";; ")
  (font-lock-fontify-buffer))

(add-to-list 'auto-mode-alist '("\\Drakefile\\'" . drakefile-mode))
(add-to-list 'auto-mode-alist '("\\.drake\\'" . drakefile-mode))
