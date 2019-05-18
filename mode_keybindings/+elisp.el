(map! :mode emacs-lisp-mode
      (:localleader
        "f"     #'eval-defun
        "%"     #'eval-buffer
        ; v for visual mode.
        "v"     #'eval-region
        "r"     #'quickrun))
