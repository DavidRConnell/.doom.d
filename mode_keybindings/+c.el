(map! :mode (c-mode objc-mode)
      (:localleader
        "r" #'quickrun
        "R" #'quickrun-shell
        :desc "Compile" "c" (lambda! (compile
                                (concat  "make run FILENAME="
                                         (file-name-sans-extension (buffer-name))
                                         " && make clean >/dev/null")))
        :desc "Debug" "d" (lambda! (gdb (concat "gdb -i=mi "
                                          (file-name-sans-extension (buffer-name)))))
        "l" #'moo-jump-local
        "L" #'moo-jump-directory))
