(map! :mode matlab-mode
      (:localleader
        "?" #'matlab-shell-describe-command
        ;; New func to first check current file for definition
        "d" #'matlab-find-file-on-path))
