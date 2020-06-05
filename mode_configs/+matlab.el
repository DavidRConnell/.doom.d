(defun insert-function-snippet ()
  "Add snippet for matlab function when opening a new .m file."
  (if (equal 0 (buffer-size))
      (insert (concat "function " (file-name-sans-extension (buffer-name)) "\nend"))))

(defun fix-matlab-imenu-generic-expression ()
    (setq imenu-generic-expression
          '(("Function" "^\\s-*\\(function\\)\\s-*\\([^\\.\n]*\\)" 2)
            ("Function" "^\\s-*\\(function\\)\\s-*\\([^\\.\n]*\\.\\.\\.\\s-*\n.*\\)" 2)
            ("Class" "^\\s-*\\(classdef\\)\\s-*\\(.*\\)" 2)
            ("Methods" "^\\s-*\\(methods\\)\\s-*\\(.*\\)?" 2)
            ("Properties" "^\\s-*\\(properties\\)\\s-*\\(.*\\)?" 2)
            ("Section" "^\\s-*%%\\s-*\\(.*\\)$" 1))))

(after! matlab
  (setq matlab-shell-command "/usr/local/MATLAB/bin/matlab")
  (setq matlab-shell-command-switches (list "-nodesktop" "-nosplash"))
  (load (concat doom-private-dir "extras/+flycheck-matlab-mlint.el"))
  (set-popup-rule! "*MATLAB*" :ignore t)
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


  (evilmi-load-plugin-rules '(matlab-mode) '(matlab))

  (add-hook! 'matlab-mode-hook
              #'display-line-numbers-mode
              #'highlight-indent-guides-mode
              #'git-gutter-mode
              #'auto-fill-mode
              #'matlab-toggle-functions-have-end
              #'insert-function-snippet
              #'fix-matlab-imenu-generic-expression
              #'yas-reload-all
              #'yas-minor-mode
              #'hl-todo-mode
              #'rainbow-delimiters-mode
              #'spell-fu-mode))
