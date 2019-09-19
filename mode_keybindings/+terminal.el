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


(add-hook! 'matlab-shell-mode-hook
  (map! :mode matlab-shell-mode
        :n "k" #'matlab-shell-previous-matching-input-from-input
        :n "j" #'matlab-shell-next-matching-input-from-input
        :n "C-p" #'matlab-shell-previous-matching-input-from-input
        :n "C-n" #'matlab-shell-next-matching-input-from-input
        :ni "C-d" #'matlab-shell-exit
        :i "C-SPC" #'matlab-shell-tab))

(add-hook! 'sly-mrepl-hook
  (map! :mode sly-mrepl-mode
        :n "k" #'sly-mrepl-previous-input-or-button
        :n "j" #'sly-mrepl-next-input-or-button
        :ni "C-p" #'sly-mrepl-previous-input-or-button
        :ni "C-n" #'sly-mrepl-next-input-or-button))
