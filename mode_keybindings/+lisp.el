;;; ~/.doom.d/mode_keybindings/+lisp.el -*- lexical-binding: t; -*-

(after! sly
  (map! :mode (lisp-mode sly-mode sly-editing-mode)
        (:prefix "C-c"
         "C-e" #'eval-defun)))
