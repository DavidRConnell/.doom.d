(map! :mode matlab-mode
      (:localleader
        "?" #'matlab-view-current-word-doc-in-another-buffer
        "d" #'matlab-jump-to-definition-of-word-at-cursor
        "r" #'matlab-shell-save-and-go
        "v" #'matlab-shell-run-region
        "C" #'matlab-shell-close-figures
        "e" #'matlab-shell-last-error
        "L" #'matlab-shell-apropos
        "l" #'flycheck-list-errors
        "s" #'imenu-list-smart-toggle)

      :i "C-SPC" #'company-matlab)
