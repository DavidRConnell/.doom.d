(load! "+binding_functions")
(if (featurep 'evil)
    (load! "+my-evil-bindings"))

(after! git-timemachine
  (add-hook 'git-timemachine-mode-hook
            #'git-timemachine-show-revision-fuzzy)
  (map! :map git-timemachine-mode-map
        "C-p" #'git-timemachine-show-previous-revision
        "C-n" #'git-timemachine-show-next-revision))

(after! term
  (add-hook 'term-mode-hook
            #'company-mode))

(after! julia-mode
  (add-hook! 'julia-mode-hook
             'julia-repl-mode))

(after! evil-magit
  (map! :mode magit-mode
        "?" #'evil-search-backward
        "K" #'evil-scroll-line-up))

(after! avy
  (setq avy-keys-alist
        '((avy-goto-char . (?a ?o ?e ?u ?h ?t ?n ?s ?-))))
  (setq avy-keys (number-sequence ?a ?z))
  (setq avy-enter-times-out nil)
  (setq avy-timeout-seconds 0.3))

(load-files-in (concat doom-private-dir "mode_keybindings"))
(load-theme doom-theme)
