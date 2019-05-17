(if (featurep 'evil)
    (load! "+my-evil-bindings"))
(load! "+matlab")

(add-hook 'git-timemachine-mode-hook
          #'git-timemachine-show-revision-fuzzy)

(add-hook 'term-mode-hook
          #'company-mode)
