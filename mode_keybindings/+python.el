(map! :mode python-mode
      :localleader
      "p" #'run-python
      "r" (lambda! (async-shell-command (concat "python3 "(buffer-file-name))))
      "v" #'python-shell-send-region
      "f" #'python-shell-send-defun
      "?" #'python-describe-at-point)
