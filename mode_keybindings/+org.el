;;; +org.el --- description -*- lexical-binding: t; -*-

(map! :mode org-mode
      :nv "C-j" #'evil-forward-section-begin
      :nv "C-k" #'evil-backward-section-begin)
