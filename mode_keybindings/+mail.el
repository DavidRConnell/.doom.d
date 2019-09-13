;;; +mail.el --- description -*- lexical-binding: t; -*-

(map! :mode notmuch-search-mode
      :n "J" #'evil-scroll-line-down

      (:localleader
        :desc "Compose mail" "c" #'compose-mail-other-window
        :desc "Reply" "r" #'notmuch-search-reply-to-thread))
