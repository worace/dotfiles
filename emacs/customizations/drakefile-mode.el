(setq drakefile-highlights
      '(("%include\\" . font-lock-function-name-face)
        ("BASE" . font-lock-constant-face)))

(defconst drakefile-syntax-table
  (let ((table (make-syntax-table)))
    ;; single and double quotes are string delimiters
    (modify-syntax-entry ?' "\"" table)
    (modify-syntax-entry ?\" "\"" table)

    ;; ; is a comment starter
    (modify-syntax-entry ?\; "<" table)
    ;; \n is a comment ender
    (modify-syntax-entry ?\n ">" table)

    table))

(defun drakefile-mode-hook ()
  (toggle-truncate-lines nil))

(define-derived-mode drakefile-mode prog-mode "drakefile-mode"
  "Major mode for working with Drakefiles: https://github.com/Factual/drake."
  :syntax-table drakefile-syntax-table
  (font-lock-fontify-buffer)
  (toggle-truncate-lines nil)
  ;; (setq font-lock-defaults '(drakefile-highlights))

  )
