;;; +org.el --- description -*- lexical-binding: t; -*-

(map! :mode org-mode
      :nv "C-j" #'org-forward-heading-same-level
      :nv "C-k" #'org-backward-heading-same-level
      :nv "H" #'evil-org-beginning-of-line
      :i "C-[" #'evil-escape-mode
      :i "ESC" #'evil-escape-mode
      (:localleader
        "n" #'org-add-note
        "p" #'org-priority
        "A" #'org-archive-subtree))

(map! :mode org-agenda-mode
      :m "J" #'evil-scroll-line-down
      :m "K" #'evil-scroll-line-up
      :m "C-j" #'org-agenda-forward-block
      :m "C-k" #'org-agenda-backward-block
      :m "/" #'org-agenda-filter-by-tag
      (:prefix "C-c"
        "C-c" #'org-agenda-set-tags
        "." #'org-agenda-date-prompt)
      (:localleader
        "n" #'org-agenda-add-note
        "p" #'org-agenda-priority
        "A" #'org-agenda-archive
        (:prefix "c"
          "c" #'org-agenda-clock-in
          "C" #'org-agenda-clock-out
          "g" #'org-agenda-clock-goto
          "t" #'org-agenda-clockreport-mode)))
