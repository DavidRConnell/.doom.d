(map! :mode emacs-lisp-mode
      :ni "C-]" #'lisp-state-toggle-lisp-state
      (:localleader
        "f"     #'eval-defun
        "%"     #'eval-buffer
        "v"     #'eval-region
        ","     #'ielm
        "r"     #'quickrun)
      (:prefix "C-c"
        "C-c" #'eval-defun
        "C-l" #'eval-buffer
        "C-e" #'eval-last-sexp))
