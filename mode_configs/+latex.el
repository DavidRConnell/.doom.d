;;; ~/.doom.d/mode_configs/+latex.el -*- lexical-binding: t; -*-

(after! latex
  (setq TeX-source-correlate-mode nil)
  (load "~/projects/syntex/syntex.el"))

(add-hook! 'LaTeX-mode-hook
           #'outline-minor-mode
           (lambda () (TeX-engine-set 'xetex)))
