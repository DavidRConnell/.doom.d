;;; +org.el --- description -*- lexical-binding: t; -*-

(map! :mode org-mode
      :nv "C-j" #'org-forward-heading-same-level
      :nv "C-k" #'org-backward-heading-same-level
      (:localleader
        "n" #'org-add-note))
