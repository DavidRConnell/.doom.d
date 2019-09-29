;;; ~/.doom.d/mode_keybindings/+julia.el -*- lexical-binding: t; -*-

(map! :mode julia-mode
      (:localleader
        "r" #'julia-repl-send-buffer
        "p" #'julia-repl
        "v" #'julia-repl-send-region-or-line
        "o" (lambda! (counsel-find-file "~/.julia/packages"))))
