;;; ~/.doom.d/mode_keybindings/+csv.el -*- lexical-binding: t; -*-

(map! :mode csv-mode
      :n "l" #'csv-forward-field
      :n "h" #'csv-backward-field)
