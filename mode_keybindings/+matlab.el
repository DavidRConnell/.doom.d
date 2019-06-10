(map! :mode matlab-mode
      (:localleader
        "?" #'matlab-view-current-word-doc-in-another-buffer
        "d" #'matlab-jump-to-definition-of-word-at-cursor
        "r" #'matlab-shell-save-and-go
        "v" #'matlab-shell-run-region
        "C" #'matlab-shell-close-figures
        "e" #'matlab-shell-last-error
        "l" #'matlab-shell-apropos
        "P" #'matlab-start
        "p" #'matlab-shell)

      :i "C-SPC" #'company-matlab)
