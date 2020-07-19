;;; ~/.doom.d/mode_keybindings/+lisp.el -*- lexical-binding: t; -*-

(after! sly
  (map! :mode (lisp-mode sly-mode sly-editing-mode)
        (:prefix "C-c"
         "C-c" (dc-arg-cmd #'sly-eval-defun
                           #'sly-compile-defun)
         "C-e" #'sly-eval-last-expression)
        (:localleader
         "'" #'nil
         "," #'sly)))
