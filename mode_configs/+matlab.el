(setq matlab-shell-command "/usr/local/MATLAB/bin/matlab")
(setq matlab-shell-command-switches (list "-nodesktop" "-nosplash"))

(defun insert-function-snippet ()
  "Add snippet for matlab function when opening a new .m file."
  (if (equal 0 (buffer-size))
      (insert (concat "function " (substring (buffer-name) 0 -2) "\nend"))))

(add-hook! 'matlab-mode-hook
  '(display-line-numbers-mode
    highlight-indent-guides-mode
    git-gutter-mode
    matlab-toggle-show-mlint-warnings
    matlab-toggle-functions-have-end
    insert-function-snippet))
