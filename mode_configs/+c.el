;;; ~/.doom.d/mode_configs/+c.el -*- lexical-binding: t; -*-

(add-hook! 'c-mode-hook
           #'function-args-mode)

(after! cc-mode
  (set-pretty-symbols! '(c-mode objc-mode)
    ;; Types
    :null "void"
    :true "true" :false "false"
    :int "int"
    :float "double"
    :str "string"
    :bool "bool"
    ;; Flow
    :not "!"
    :and "&&" :or "||"
    :for "for"
    :return "return"
    :yield "#require"
    :not-equal "!="
    :gt-equal ">="
    :lt-equal "<="))
