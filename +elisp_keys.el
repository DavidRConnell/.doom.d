(map! :mode elisp-mode
      (:localleader
        "e"     #'eval-defun
        "%"     #'eval-buffer
        ; v for visual mode.
        "v"     #'eval-region
        "r"     #'quickrun))
