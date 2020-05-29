(map! :mode (c-mode objc-mode)
      (:localleader
        "r" #'quickrun
        "R" #'quickrun-shell
        :desc "Compile" "c" (cmd! (compile
                                (concat  "make run FILENAME="
                                         (file-name-sans-extension (buffer-name))
                                         " && make clean >/dev/null")))
        :desc "Debug" "d" (cmd! (gdb (concat "gdb -i=mi "
                                          (file-name-sans-extension (buffer-name)))))
        "l" #'moo-jump-local
        "L" #'moo-jump-directory
        "h" #'dc-toggle-header-file))

(defun dc-toggle-header-file ()
  (interactive)
  (let* ((fullname (buffer-name))
         (ext (file-name-extension fullname))
         (name (file-name-sans-extension fullname)))
    (cond ((and (string= ext "c") (file-exists-p (concat name ".h")))
           (find-file (concat name ".h")))
          ((and (string= ext "h") (file-exists-p (concat name ".c")))
           (find-file (concat name ".c"))))))
