;;; ~/.doom.d/mode_keybindings/+sh.el -*- lexical-binding: t; -*-

(map! :mode sh-mode
      (:localleader
       "r" #'quickrun
       "R" #'quickrun-shell))
