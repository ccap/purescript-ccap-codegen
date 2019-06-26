;; --------------------------------------------------------------------
;; Customization

(defcustom ccap-temple-mode-codegen-repo ""
  "Path to the codegen repository directory."
  :group 'ccap-temple-mode
  :type 'string)

(defconst ccap-temple-out-buffer (generate-new-buffer "*ccap-temple output*"))

;; --------------------------------------------------------------------
;; Helpers

(defun ccap-temple-current-line ()
  "Get the current line as text."
  (buffer-substring-no-properties
   (line-beginning-position)
   (line-end-position)))

(defun ccap-temple-get-main-dir (tmpl-filename)
  "Given a TMPL-FILENAME, get the directory for the main src folder.
Assumes that the tmpl file is in <project-dir>/main/resources/."
  (car (split-string tmpl-filename "/resources")))

(defun ccap-temple-module-to-path (tmpl-filename lang)
  "Given a TMPL-FILENAME produce the path for the LANG source directory."
  (let ((src-dir (if (string= lang "purs")
                     "purescript"
                   "scala")))
    (concat
     (ccap-temple-get-main-dir tmpl-filename)
     "/"
     src-dir)))

(defun ccap-temple-parse-line-target (line lang)
  "Parse LINE for a LANG target annotation in a tmpl file."
  (let ((annotation (concat "//" lang "-target: ")))
    (when (string-match annotation line)
      (replace-regexp-in-string annotation "" line))))

(defun ccap-temple-buffer-get-target (lang)
  "Parse the current buffer for the LANG target annotation."
  (let ((target nil))
    (save-excursion
      (beginning-of-buffer)
      (while
          (and
           (not target)
           ;; Not at end of buffer
           (< (point) (point-max)))
        (let ((parse (ccap-temple-parse-line-target
                           (ccap-temple-current-line)
                           lang)))
          (if parse
              (setq target parse)
            (forward-line)))))
    target))

(defun ccap-temple-compile-command (compiler-loc target-package language out-dir)
  "Construct the console command to run the compiler.
COMPILER-LOC the directory containing the compiler executable.
TARGET-PACKAGE package or module to place the generated module.
LANGUAGE either 'scala' or 'purs'.
OUT-DIR directory to place the generated code file."
  (let ((compile-command (concat compiler-loc "/compile"))
        (module-argument (concat "-p " target-package))
        (lang-argument (concat "-m " language))
        (output-argument (concat "-o " out-dir))
        (input-argument (buffer-file-name)))
    (string-join
     (list compile-command
           module-argument
           lang-argument
           output-argument
           input-argument)
     " ")))

;; --------------------------------------------------------------------
;; Commands

(defun ccap-temple-mode-compile ()
  "Compile the current buffer."
  (interactive)
  (if (string= ccap-temple-mode-codegen-repo "")
      (message "ccap-temple-mode-codegen-repo unset. Cannot compile.")
    (let ((purs-target (ccap-temple-buffer-get-target "purs"))
          (scala-target (ccap-temple-buffer-get-target "scala")))
      (if (and purs-target scala-target)
          (let ((purs-command (ccap-temple-compile-command
                               ccap-temple-mode-codegen-repo
                               purs-target
                               "purs"
                               (ccap-temple-module-to-path
                                (buffer-file-name)
                                "purs")))
                (scala-command (ccap-temple-compile-command
                                ccap-temple-mode-codegen-repo
                                scala-target
                                "scala"
                                (ccap-temple-module-to-path
                                 (buffer-file-name)
                                 "scala"))))
            (shell-command purs-command)
            (shell-command scala-command))
        (cond
         ((and (not scala-target) (not purs-target))
          (message "Could not detect purescript or scala targets. Cannot compile."))
         ((not purs-target)
          (message "Could not detect pursecript target. Cannot compile."))
         ((not scala-target)
          (message "Could not detect scala target. Cannot compile.")))))))

;; --------------------------------------------------------------------
;; Keybindings

(defvar ccap-temple-mode-map nil
  "Keymap for ccap-temple-mode")

(setq ccap-temple-mode-map (make-sparse-keymap))

(define-key ccap-temple-mode-map (kbd "C-c C-c") 'ccap-temple-mode-compile)

;; --------------------------------------------------------------------
;; Syntax highlighting

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
