;;; +mail.el --- description -*- lexical-binding: t; -*-

(after! mu4e
  (map! :mode mu4e-headers-mode
        :n "J" #'evil-scroll-line-down
        :n "K" #'evil-scroll-line-up
        :n "gr" #'mu4e-update-index
        (:prefix "C-c"
          :n "C-j" #'mu4e~headers-jump-to-maildir))

  (map! :mode mu4e-main-mode
        (:prefix "C-c"
          :n "C-j" #'mu4e~headers-jump-to-maildir))

  (map! :mode mu4e-view-mode
        :n "J" #'evil-scroll-line-down
        :n "K" #'evil-scroll-line-up
        :n "C-n" #'mu4e-view-headers-next
        :n "C-p" #'mu4e-view-headers-prev
        (:prefix "C-c"
          :n "C-j" #'mu4e~headers-jump-to-maildir)))
