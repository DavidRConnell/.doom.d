;;; ~/.doom.d/mode_configs/+latex.el -*- lexical-binding: t; -*-

(after! latex
  (setq-default TeX-engine 'xetex)
  (setq TeX-source-correlate-mode nil)
  (load "~/projects/syntex/syntex.el"))
