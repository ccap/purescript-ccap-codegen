(defvar ccap-temple-mode-syntax-table nil
  "Syntax table for `ccap-temple-mode'.")

;; Will be automagically detected because of uniform naming w/ the mode.
(setq ccap-temple-mode-syntax-table
      (let ((syn-table (make-syntax-table)))
        ;; the `n' means that comments can be nested
        (modify-syntax-entry ?\/  ". 124b" syn-table)
        (modify-syntax-entry ?\*  ". 23n" syn-table)
        (modify-syntax-entry ?\n  "> b" syn-table)
        (modify-syntax-entry ?\r "> b" syn-table)
        syn-table))

;; Create the list for font-lock.
;; Each category of keywords is given a particular face.
(setq ccap-temple-font-lock-keywords
      (let* (;; Define several categories of keywords
             (x-keywords '("module" "type"))
             (x-types '("int" "string" "decimal" "boolean" "array"))
             (x-functions '("wrap" "optional"))

             ;; Generate regex strings to parse each kategory of keywords
             (x-keywords-regexp (regexp-opt x-keywords 'words))
             (x-types-regexp (regexp-opt x-types 'words))
             (x-functions-regexp (regexp-opt x-functions 'words)))
        `((,x-keywords-regexp . font-lock-keyword-face)
          (,x-types-regexp . font-lock-type-face)
          (,x-functions-regexp . font-lock-function-name-face))))

;;;###autoload
(define-derived-mode ccap-temple-mode fundamental-mode "ccap-temple"
  "Major mode for editing CCAP code-gen template files."

  ;; Syntax highlighting.
  (setq font-lock-defaults '((ccap-temple-font-lock-keywords))))

;; Add this mode to the `features' list.
(provide 'ccap-temple-mode)
