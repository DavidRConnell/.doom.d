(defun insert-function-snippet ()
  "Add snippet for matlab function when opening a new .m file."
  (if (equal 0 (buffer-size))
      (insert (concat "function " (substring (buffer-name) 0 -2) "\nend"))))

(defun fix-matlab-imenu-generic-expression ()
    (setq imenu-generic-expression
          '(("Function" "^\\s-*\\(function\\)\\s-*\\([^\\.\n]*\\)\n" 2)
            ("Function" "^\\s-*\\(function\\)\\s-*\\([^\\.]*\\.\\.\\.\\s-*\n.*\\)" 2)
            ("Class" "^\\s-*\\(classdef\\)\\s-*\\(.*\\)" 2)
            ("Section" "%%\\s-*\\(.*\\)$" 1))))

(after! matlab
  (setq matlab-shell-command "/usr/local/MATLAB/bin/matlab")
  (setq matlab-shell-command-switches (list "-nodesktop" "-nosplash"))
  (load (concat doom-private-dir "extras/+company-matlab.el"))
  (load (concat doom-private-dir "extras/+flycheck-matlab-mlint.el"))
  (setq flycheck-matlab-mlint-executable
        "/usr/local/MATLAB/bin/glnxa64/mlint")

  (set-pretty-symbols! '(matlab-mode)
    :def "function"
    ;; Types
    :true "true" :false "false"
    ;; Flow
    :not "~"
    :and "&&" :or "||"
    :for "for"
    ;; :return "return"
    :not-equal "~="
    :gt-equal ">="
    :lt-equal "<=")


  (add-hook! 'matlab-mode-hook
              #'display-line-numbers-mode
              #'highlight-indent-guides-mode
              #'git-gutter-mode
              #'auto-fill-mode
              #'matlab-toggle-functions-have-end
              #'insert-function-snippet
              #'fix-matlab-imenu-generic-expression
              #'yas-minor-mode
              #'hl-todo-mode
              #'rainbow-delimiters-mode))

(after! (:and evil-matchit matlab)
  (evilmi-load-plugin-rules '(matlab-mode) '(matlab)))
