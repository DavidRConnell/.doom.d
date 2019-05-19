(map! :mode matlab-mode
      (:localleader
        "?" #'matlab-view-current-word-doc-in-another-buffer
        "d" #'matlab-jump-to-definition-of-word-at-cursor
        "r" #'matlab-shell-save-and-go
        "v" #'matlab-shell-run-region
        "l" #'matlab-shell-apropos
        "C" #'matlab-shell-close-figures
        "e" #'matlab-shell-last-error)

      :i "C-SPC" #'company-matlab)
