;;; ~/.doom.d/mode_keybindings/+gud.el -*- lexical-binding: t; -*-

(map! :mode gud-mode
      (:localleader
        "w" #'gdb-many-windows))
