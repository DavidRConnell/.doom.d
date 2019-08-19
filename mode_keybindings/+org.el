;;; +org.el --- description -*- lexical-binding: t; -*-

(map! :mode org-mode
      :nv "C-j" #'org-forward-heading-same-level
      :nv "C-k" #'org-backward-heading-same-level
      :nv "H" #'evil-org-beginning-of-line
      :i "C-[" #'evil-escape-mode
      :i "ESC" #'evil-escape-mode
      (:localleader
        "n" #'org-add-note
        "p" #'org-priority))
