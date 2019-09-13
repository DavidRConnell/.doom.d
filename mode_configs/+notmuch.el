;;; +notmuch.el --- description -*- lexical-binding: t; -*-

(setq +notmuch-sync-backend 'offlineimap)
(setq +notmuch-mail-folder "~/mail/")
(autoload 'notmuch "notmuch" "notmuch mail" t)
