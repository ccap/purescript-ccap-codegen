To use, add this code to your .init.el or .spacemacs file:

```
;; CCAP code gen
(load "~/.emacs.d/lisp/ccap-code-gen/ccap-temple-mode.el")
(add-to-list 'auto-mode-alist '("\\.tmpl\\'" . ccap-temple-mode))
(add-hook 'ccap-temple-mode-hook
          #'(lambda ()
              (setq-local
               ccap-temple-mode-codegen-repo
               "~/<path-to-your-repo>/purescript-ccap-codegen")))
```

ccap-temple-mode has flycheck support. If you have flycheck installed, you can turn flycheck on in .tmpl files by adding this line after the above:

```
(add-hook 'ccap-temple-mode-hook 'flycheck-mode)
```
