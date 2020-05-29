;;; ~/.doom.d/mode_keybindings/+julia.el -*- lexical-binding: t; -*-

(map! :mode julia-mode
      (:localleader
        "?" #'julia-repl-doc
        "r" #'julia-repl-send-buffer
        "p" #'julia-repl
        "v" #'julia-repl-send-region-or-line
        "o" (cmd! (dc-open-in-workspace "Julia Package" "~/.julia/packages")))
      (:prefix "C-c"
        "C-c" #'julia-repl-send-region-or-line
        "C-l" #'julia-repl-send-buffer))
