(map! :mode term-mode
      :n "j" #'term-send-down
      :n "k" #'term-send-up
      :i "C-p" #'term-send-up
      :i "C-n" #'term-send-down
      :n "g" #'term-send-home
      :n "C-d" #'term-send-eof)
