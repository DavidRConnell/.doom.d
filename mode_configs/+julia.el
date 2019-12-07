;;; ~/.doom.d/mode_configs/+julia.el -*- lexical-binding: t; -*-

(after! julia-menu
  (setq julia-imenu-generic-expression
        '(("Function (_)" "[ 	]*function[ 	]+\\(_[^ 	\n]*\\)" 1)
          ("Function" "^[ 	]*function[ 	]+\\([^_][^	\n]*\\)" 1)
          ("Function" "^[ \t]*\\([a-zA-Z0-9]*(.*)[ \t]*=.*\\)" 1)
          ("Struct" "struct[ \t]*\\(.*\\)")
          ("Const" "[ 	]*const \\([^ 	\n]*\\)" 1)
          ("Type" "^[ 	]*[a-zA-Z0-9_]*type[a-zA-Z0-9_]* \\([^ 	\n]*\\)" 1)
          ("Require" " *\\(\\brequire\\)(\\([^ 	\n)]*\\)" 2)
          ("Include" " *\\(\\binclude\\)(\\([^ 	\n)]*\\)" 2)
          ("Using" "^ *\\(using\\)[ \t]*\\(.*\\)$" 2))))
