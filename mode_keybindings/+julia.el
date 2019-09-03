;;; ~/.doom.d/mode_keybindings/+julia.el -*- lexical-binding: t; -*-

(map! :mode julia-mode
      (:localleader
        "r" #'quickrun
        "p" #'run-julia))
