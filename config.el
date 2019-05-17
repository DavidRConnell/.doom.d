(if (featurep 'evil)
    (load! "+my-evil-bindings"))
(load! "+matlab")

(add-hook 'git-timemachine-mode-hook
          #'git-timemachine-show-revision-fuzzy)
