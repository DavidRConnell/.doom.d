(map! :mode c-mode
      (:localleader
        "r" #'quickrun
        "R" #'quickrun-shell
        "c" (lambda! (compile (concat  "make run FILENAME="
                                         (file-name-sans-extension (buffer-name))
                                         " && make clean >/dev/null")))
        "d" (lambda! (gdb (concat "gdb -i=mi "
                                  (file-name-sans-extension (buffer-name)))))
        "l" #'moo-jump-local
        "L" #'moo-jump-directory))
