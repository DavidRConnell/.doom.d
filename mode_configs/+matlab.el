(defun insert-function-snippet ()
  "Add snippet for matlab function when opening a new .m file."
  (if (equal 0 (buffer-size))
      (insert (concat "function " (substring (buffer-name) 0 -2) "\nend"))))

(defun fix-matlab-imenu-generic-expression ()
    (setq imenu-generic-expression
          '((nil "^\\s-*\\(function\\)\\s-*\\(.*\\)" 2))))

(after! matlab
  (setq matlab-shell-command "/usr/local/MATLAB/bin/matlab")
  (setq matlab-shell-command-switches (list "-nodesktop" "-nosplash"))
  (load (concat doom-private-dir "extras/+company-matlab.el"))
  (load (concat doom-private-dir "extras/+flycheck-matlab-mlint.el"))
  (setq flycheck-matlab-mlint-executable
        "/usr/local/MATLAB/bin/glnxa64/mlint")

  (add-hook 'matlab-shell-mode-hook
            #'company-mode)

  (add-hook! 'matlab-mode-hook
              #'display-line-numbers-mode
              #'highlight-indent-guides-mode
              #'git-gutter-mode
              #'matlab-toggle-functions-have-end
              #'insert-function-snippet
              #'fix-matlab-imenu-generic-expression
              #'yas-minor-mode))
