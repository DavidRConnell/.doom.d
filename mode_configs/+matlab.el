(defcustom default-fill-column 80
  "Dummy var so matlab-mode works"
  :type 'integer)

(defun string-to-int (str)
  "So matlab-mode works"
  (string-to-number str))

(require 'matlab-mode)
(setq matlab-server-executable "/usr/local/MATLAB/bin/matlab")
(matlab-mode-common-setup)
(setq matlab-shell-command "/usr/local/MATLAB/bin/matlab")
(setq matlab-shell-command-switches (list "-nodesktop" "-nosplash"))

(defun insert-function-snippet ()
  (if (equal 0 (buffer-size))
      (insert (concat "function " (substring (buffer-name) 0 -2) "\nend"))))

(add-hook! 'matlab-mode-hook
  '(display-line-numbers-mode
    highlight-indent-guides-mode
    git-gutter-mode
    insert-function-snippet))
