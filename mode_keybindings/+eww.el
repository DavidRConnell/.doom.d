;;; ~/.doom.d/mode_keybindings/+eww.el -*- lexical-binding: t; -*-

(map! :mode eww-mode
      (:localleader
       "y" #'eww-copy-page-url)
      :n "C-o" #'eww-back-url
      :n "C-i" #'eww-forward-url)
