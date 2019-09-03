;;; ~/.doom.d/mode_keybindings/+latex.el -*- lexical-binding: t; -*-

(map! :mode latex-mode
      :nvi "\\t" #'flyspell-correct-word-generic)
