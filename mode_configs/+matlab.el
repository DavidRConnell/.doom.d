(require 'matlab-mode)
(setq matlab-server-executable "~/MATLAB/bin/matlab")
(matlab-mode-common-setup)
(setq matlab-shell-command "~/MATLAB/bin/matlab")
(setq matlab-shell-command-switches (list "-nodesktop" "-nosplash"))

(defcustom default-fill-column 80
  "Dummy var so matlab-mode works"
  :type 'integer)

(add-hook! 'matlab-mode-hook
  '(display-line-numbers-mode
    highlight-numbers-mode
    highlight-indent-guides-mode
    git-gutter-mode))
