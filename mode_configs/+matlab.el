(require 'matlab-mode)
(setq matlab-server-executable "/usr/local/MATLAB/R2019a/bin/matlab")
(matlab-mode-common-setup)
(setq matlab-shell-command "/usr/local/MATLAB/R2019a/bin/matlab")
(setq matlab-shell-command-switches (list "-nodesktop" "-nosplash"))

(defcustom default-fill-column 80
  "Dummy var so matlab-mode works"
  :type 'integer)

(add-hook! 'matlab-mode-hook
  '(display-line-numbers-mode
    highlight-numbers-mode
    highlight-indent-guides-mode
    git-gutter-mode))
