;;; ~/.doom.d/mode_keybindings/+latex.el -*- lexical-binding: t; -*-

(map! :mode LaTeX-mode
      :nvi [tab] #'flyspell-correct-word-generic
      :n "C-SPC" #'flyspell-correct-word-generic
      (:localleader
        :desc "Insert figure" "f" #'dc-insert-latex-figure
        :desc "Insert table" "t" #'dc-insert-latex-table
        :desc "Insert section" "s" #'dc-insert-latex-section
        :desc "Insert environment" "e" #'LaTeX-environment))
