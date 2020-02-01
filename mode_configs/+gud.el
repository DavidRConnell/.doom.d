;;; ~/.doom.d/mode_configs/+gud.el -*- lexical-binding: t; -*-

(add-hook! 'gud-mode-hook
           #'company-mode)
