(map! :mode term-mode
      :n "j" #'term-send-down
      :n "k" #'term-send-up
      :ni "C-p" #'term-send-up
      :ni "C-n" #'term-send-down
      :n "G" #'term-send-home
      :n "C-d" #'term-send-eof)

(add-hook! 'eshell-mode-hook
  (map! :mode eshell-mode
        :ni "C-d" #'+eshell/kill-and-close
        :ni "C-p" #'eshell-previous-input
        :ni "C-n" #'eshell-next-input
        :n "k" #'eshell-previous-matching-input-from-input
        :n "j" #'eshell-next-matching-input-from-input))

(add-hook! 'sly-mrepl-hook
  (map! :mode sly-mrepl-mode
        :n "k" #'sly-mrepl-previous-input-or-button
        :n "j" #'sly-mrepl-next-input-or-button
        :ni "C-p" #'sly-mrepl-previous-input-or-button
        :ni "C-n" #'sly-mrepl-next-input-or-button))
