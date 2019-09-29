;;; +mail.el --- description -*- lexical-binding: t; -*-

(map! :mode mu4e-headers-mode
      :n "J" #'evil-scroll-line-down
      :n "K" #'evil-scroll-line-up
      (:prefix "C-c"
        :n "C-j" #'mu4e~headers-jump-to-maildir))

(map! :mode mu4e-view-mode
      :n "J" #'evil-scroll-line-down
      :n "K" #'evil-scroll-line-up
      :n "C-n" #'mu4e-view-headers-next
      :n "C-p" #'mu4e-view-headers-prev
      (:prefix "C-c"
        :n "C-j" #'mu4e~headers-jump-to-maildir))
