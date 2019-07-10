(map! :mode term-mode
      :n "j" #'term-send-down
      :n "k" #'term-send-up
      :i "C-p" #'term-send-up
      :i "C-n" #'term-send-down
      :n "g" #'term-send-home
      :n "C-d" #'term-send-eof)

(add-hook! 'matlab-shell-mode-hook
          (map! :mode matlab-shell-mode
                :n "k" #'matlab-shell-previous-matching-input-from-input
                :n "j" #'matlab-shell-next-matching-input-from-input
                :ni "C-k" #'previous-line
                :i "C-p" #'matlab-shell-previous-matching-input-from-input
                :i "C-n" #'matlab-shell-next-matching-input-from-input
                :ni "C-d" #'matlab-shell-exit))
