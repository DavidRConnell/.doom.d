;;; ~/.doom.d/mode_keybindings/+latex.el -*- lexical-binding: t; -*-

(after! latex
  (map! :mode LaTeX-mode
        :i [tab] #'flyspell-correct-word-generic
        (:localleader
          :desc "Insert figure" "f" #'syntex-insert-figure
          :desc "Insert subfigures" "F" #'syntex-insert-subfigure
          :desc "Insert table" "t" #'syntex-insert-table
          :desc "Insert section" "s" #'syntex-insert-section
          :desc "Insert environment" "e" #'LaTeX-environment
          :desc "Cite" "c" #'syntex-cite
          :desc "Bold" "B" #'syntex-insert-bold
          :desc "Emphasize" "b" #'syntex-insert-emph
          :desc "Italicize" "i" #'syntex-insert-italic
          :desc "label" "l" #'syntex-insert-label
          :desc "Go to main" "m" #'syntex-goto-main)))
