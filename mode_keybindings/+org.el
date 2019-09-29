;;; +org.el --- description -*- lexical-binding: t; -*-

(map! :map org-mode-map
      :nv "C-j" #'org-forward-heading-same-level
      :nv "C-k" #'org-backward-heading-same-level
      :nv "H" #'org-beginning-of-line
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
      :m "L" #'evil-end-of-line
      :m "H" #'evil-beginning-of-line
      :m "RET" #'+org/dwim-at-point
      (:prefix "C-c"
        "C-c" #'org-agenda-set-tags
        "." #'org-agenda-date-prompt)
      (:localleader
        "n" #'org-agenda-add-note
        "p" #'org-agenda-priority
        "A" #'org-agenda-archive
        (:prefix ("c" . "Clock")
          "c" #'org-agenda-clock-in
          "C" #'org-agenda-clock-out
          "g" #'org-clock-goto
          "r" #'org-agenda-clockreport-mode)))
