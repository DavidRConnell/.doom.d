(map! :mode python-mode
      :localleader
      "p" #'run-python
      "r" #'quickrun
      "v" #'python-shell-send-region
      "f" #'python-shell-send-defun
      "l" #'flycheck-list-errors
      "?" #'python-describe-at-point
      "s" #'imenu-list-smart-toggle)
