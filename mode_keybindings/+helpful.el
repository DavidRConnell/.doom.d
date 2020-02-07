;;; ~/.doom.d/mode_keybindings/+helpful.el -*- lexical-binding: t; -*-

(map! :mode (helpful-mode Info-mode)
      (:prefix "C-c"
        "C-c" #'eval-defun
        "C-e" #'eval-last-sexp))
