;; (setq default-fill-column 80)
;; (defun string-to-int (str)
;;   (string-to-number str))

(if (featurep 'evil)
    (load! "+my-evil-bindings"))

(add-hook 'git-timemachine-mode-hook
          #'git-timemachine-show-revision-fuzzy
          (map! :mode git-timemachine-mode
                :ni "C-p" #'git-timemachine-show-previous-revision
                :ni "C-n" #'git-timemachine-show-next-revision))

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


(load-files-in (concat doom-private-dir "mode_keybindings"))
(load-theme doom-theme)
