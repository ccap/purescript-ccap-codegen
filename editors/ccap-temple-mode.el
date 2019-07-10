;; --------------------------------------------------------------------
;; Customization

(defcustom ccap-temple-mode-codegen-repo ""
  "Path to the codegen repository directory."
  :group 'ccap-temple-mode
  :type 'string)

(defcustom ccap-temple-mode-tab-width 2
  "Length of an indentation level."
  :group 'ccap-temple-mode
  :type 'number)

(defconst ccap-temple-out-buffer (generate-new-buffer "*ccap-temple output*"))

;; --------------------------------------------------------------------
;; Helpers

(defun ccap-temple-directory-splat ()
  "Get a filename that matches all files in the current directory.
EG. /home/steve/documents/test.txt -> /home/steve/documents/*"
  (replace-regexp-in-string
   (rx "/" (one-or-more (not (any "/"))) eol)
   "/*"
   (buffer-file-name)))

(defun ccap-temple-current-line ()
  "Get the current line as text."
  (buffer-substring-no-properties
   (line-beginning-position)
   (line-end-position)))

(defun ccap-temple-trim-line ()
  "Remove whitespace from the start of current line."
  (save-excursion
    (beginning-of-line)
    (while (and (char-equal (following-char) ?\s)
                (not (string= (ccap-temple-current-line) "")))
      (delete-char 1))))

(defun ccap-temple-clean-line (line)
  "Strip trailing whitespace and comments from LINE.
Strips trailing whitespace after stripping comments."
  (string-trim-right
   (replace-regexp-in-string (rx "//" (* any))
                             ""
                             (replace-regexp-in-string (rx "/*" (* any) "*/")
                                                       ""
                                                       line))))

(defun ccap-temple-increments-indentation-p (line)
  "Return t if LINE should increment the indentation level."
  (let ((clean-line (ccap-temple-clean-line line)))
    (when (not (string= clean-line ""))
      (char-equal ?{
                  (aref clean-line (1- (string-width clean-line)))))))

(defun ccap-temple-decrements-self-indentation-p (line)
  "Return t if LINE should decrement the indentation level on that line."
  (let ((clean-line (ccap-temple-clean-line line)))
    (when (not (string= clean-line ""))
      (char-equal ?}
                  (aref clean-line (1- (string-width clean-line)))))))

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

(defun ccap-temple-target-filename (base-path lang target module)
  "Given all the pieces, create a filename for a target file in LANG."
  (string-join
   (list base-path
         (replace-regexp-in-string (rx ".")
                                   "/"
                                   target)
         (concat module "." lang))
   "/"))

(defun ccap-temple-buffer-get-module ()
  "Get the module defined in the buffer."
  (let ((module nil))
    (save-excursion
      (beginning-of-buffer)
      (while
          (and
           (not module)
           ;; Not at end of buffer
           (< (point) (point-max)))
        (let* ((line (ccap-temple-clean-line (ccap-temple-current-line)))
               (module-parsed (second
                               (s-match (rx "module "
                                            (group (one-or-more word)))
                                        line))))
          (if module-parsed
              (setq module module-parsed)
            (forward-line)))))
    module))

(defun ccap-temple-compile-command (compiler-loc target-package language out-dir)
  "Construct the console command to run the compiler on every file in current directory.
COMPILER-LOC the directory containing the compiler executable.
TARGET-PACKAGE package or module to place the generated module.
LANGUAGE either 'scala' or 'purs'.
OUT-DIR directory to place the generated code file."
  (let ((compile-command (concat compiler-loc "/compile"))
        (module-argument (concat "-p " target-package))
        (lang-argument (concat "-m " language))
        (output-argument (concat "-o " out-dir))
        (input-argument (ccap-temple-directory-splat)))
    (string-join
     (list compile-command
           module-argument
           lang-argument
           output-argument
           input-argument
           ;; Run the command in the background to not block Emacs
           "&")
     " ")))

(defun ccap-temple-jump-to-lang (lang)
  "Try to jump to the target LANG file."
  (let ((module (ccap-temple-buffer-get-module))
        (target (ccap-temple-buffer-get-target lang)))
    (cond
     ((not module)
      (message "Could not detect module."))
     ((not target)
      (message (concat "Could not detect " lang " target.")))
     (t
      (find-file (ccap-temple-target-filename
                  (ccap-temple-module-to-path (buffer-file-name) lang)
                  lang
                  target
                  module))))))

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
            (let ((command (concat purs-command " " scala-command)))
              (print (concat "Compiling: " command) ccap-temple-out-buffer)
              (shell-command command)))
        (cond
         ((and (not scala-target) (not purs-target))
          (message "Could not detect purescript or scala targets. Cannot compile."))
         ((not purs-target)
          (message "Could not detect pursecript target. Cannot compile."))
         ((not scala-target)
          (message "Could not detect scala target. Cannot compile.")))))))

;; TODO: Properly indent the <> annotation syntax
(defun ccap-temple-mode-indent ()
  "Indent the current line."
  (interactive)
  ;; If at start of file, indent to 0.
  (let* ((self-line (ccap-temple-current-line))
         (clean-self-line (ccap-temple-clean-line self-line))
         (indentation-lvl (when (= (point-min) (line-beginning-position))
                            0)))
    (ccap-temple-trim-line)
    (beginning-of-line)
    (save-excursion
      (while (not indentation-lvl)
        (forward-line -1)
        (if (= (point) (point-min))
            (setq indentation-lvl 0)
          (let* ((line (ccap-temple-current-line))
                 (clean-line (ccap-temple-clean-line line)))
            ;; If the next previous line has content, check its indentation level.
            (when (not (string= (string-trim-right line) ""))
              (cond
               ((and (ccap-temple-increments-indentation-p clean-line)
                     (ccap-temple-decrements-self-indentation-p clean-self-line))
                (setq indentation-lvl (current-indentation)))
               ((ccap-temple-increments-indentation-p clean-line)
                (setq indentation-lvl (+ ccap-temple-mode-tab-width (current-indentation))))
               ((ccap-temple-decrements-self-indentation-p clean-self-line)
                (setq indentation-lvl (- (current-indentation) ccap-temple-mode-tab-width)))
               (t (setq indentation-lvl (current-indentation)))))))))
    (indent-to indentation-lvl)))

(defun ccap-temple-jump-to-scala ()
  "Jump to the scala target file."
  (interactive)
  (ccap-temple-jump-to-lang "scala"))

(defun ccap-temple-jump-to-purs ()
  "Jump to the purescript target file."
  (interactive)
  (ccap-temple-jump-to-lang "purs"))

;; --------------------------------------------------------------------
;; Flycheck

(flycheck-define-checker ccap-temple
  "A .tmpl file syntax checker using the configured compiler."
  ;; Flycheck tries really hard to force you to hard-code the location of the compiler.
  ;; We want to load it from ccap-temple-mode-codegen-repo, so we'll just manually set
  ;; the variable that's supposed to be for user customization when we init the mode.
  :command ("REPLACE_ME"
            "-m"
            "pretty"
            "-p"
            "test"
            source)
  :standard-input t
  :error-patterns
  ((error "ERROR: Could not parse " (file-name) ": line " line ", column " column
          ": " (message) line-end))
  :modes ccap-temple-mode)

;; --------------------------------------------------------------------
;; Keybindings

(defvar ccap-temple-mode-map nil
  "Keymap for ccap-temple-mode")

(setq ccap-temple-mode-map (make-sparse-keymap))

(define-key ccap-temple-mode-map (kbd "C-c C-c") 'ccap-temple-mode-compile)
(define-key ccap-temple-mode-map (kbd "C-j s") 'ccap-temple-jump-to-scala)
(define-key ccap-temple-mode-map (kbd "C-j p") 'ccap-temple-jump-to-purs)

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
             (x-types '("Int" "String" "Decimal" "Boolean" "Array"))
             (x-functions '("wrap" "Maybe"))

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

  ;; Indentation
  (setq indent-line-function #'ccap-temple-mode-indent)

  ;; Flycheck
  (add-to-list 'flycheck-checkers 'ccap-temple)
  (flycheck-add-mode 'ccap-temple 'ccap-temple-mode)
  (setq flycheck-ccap-temple-executable (concat ccap-temple-mode-codegen-repo "/compile"))

  ;; Syntax highlighting.
  (setq font-lock-defaults '((ccap-temple-font-lock-keywords))))

;; Add this mode to the `features' list.
(provide 'ccap-temple-mode)
