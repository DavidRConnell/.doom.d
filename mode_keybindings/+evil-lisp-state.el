;;; ~/.doom.d/mode_keybindings/+evil-lisp-state.el -*- lexical-binding: t; -*-

(map!
 :after evil-lisp-state
 :map evil-lisp-state-map
 "%"   #'evil-lisp-state-evil-jump-item
 ":"   #'evil-lisp-state-evil-ex
 "("   #'evil-lisp-state-insert-sexp-before
 ")"   #'evil-lisp-state-insert-sexp-after
 "` k" #'evil-lisp-state-sp-kill-hybrid-sexp
 "` p" #'evil-lisp-state-sp-push-hybrid-sexp
 "` s" #'evil-lisp-state-sp-slurp-hybrid-sexp
 "` t" #'evil-lisp-state-sp-transpose-hybrid-sexp
 "0"   #'evil-lisp-state-beginning-of-sexp
 "a"   #'evil-lisp-state-sp-absorb-sexp
 "b"   #'evil-lisp-state-sp-forward-barf-sexp
 "B"   #'evil-lisp-state-sp-backward-barf-sexp
 "c"   #'evil-lisp-state-sp-convolute-sexp
 "ds"  #'evil-lisp-state-sp-kill-symbol
 "Ds"  #'evil-lisp-state-sp-backward-kill-symbol
 "dw"  #'evil-lisp-state-sp-kill-word
 "Dw"  #'evil-lisp-state-sp-backward-kill-word
 "dx"  #'evil-lisp-state-sp-kill-sexp
 "Dx"  #'evil-lisp-state-sp-backward-kill-sexp
 "e"   #'evil-lisp-state-sp-splice-sexp-killing-forward
 "E"   #'evil-lisp-state-sp-splice-sexp-killing-backward
 "h"   #'evil-lisp-state-sp-backward-symbol
 "H"   #'evil-lisp-state-sp-backward-sexp
 "i"   #'evil-lisp-state-evil-insert-state
 "I"   #'evil-lisp-state-evil-insert-line
 "j"   #'evil-lisp-state-next-closing-paren
 "J"   #'evil-lisp-state-sp-join-sexp
 "gj"  #'join-line
 "k"   #'evil-lisp-state-prev-opening-paren
 "l"   #'evil-lisp-state-forward-symbol
 "L"   #'evil-lisp-state-sp-forward-sexp
 "p"   #'evil-lisp-state-evil-paste-after
 "P"   #'evil-lisp-state-evil-paste-before
 "r"   #'evil-lisp-state-sp-raise-sexp
 "s"   #'evil-lisp-state-sp-forward-slurp-sexp
 "S"   #'evil-lisp-state-sp-backward-slurp-sexp
 "t"   #'evil-lisp-state-sp-transpose-sexp
 "u"   #'evil-lisp-state-undo-tree-undo
 "U"   #'evil-lisp-state-sp-backward-up-sexp
 "C-r" #'evil-lisp-state-undo-tree-redo
 "v"   #'evil-lisp-state-evil-visual-char
 "V"   #'evil-lisp-state-evil-visual-line
 "C-v" #'evil-lisp-state-evil-visual-block
 "w"   #'evil-lisp-state-wrap
 "W"   #'evil-lisp-state-sp-unwrap-sexp
 "y"   #'evil-lisp-state-sp-copy-sexp
 "1"   #'digit-argument
 "2"   #'digit-argument
 "3"   #'digit-argument
 "4"   #'digit-argument
 "5"   #'digit-argument
 "6"   #'digit-argument
 "7"   #'digit-argument
 "8"   #'digit-argument
 "9"   #'digit-argument)
