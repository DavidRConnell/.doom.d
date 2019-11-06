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
  (add-hook 'julia-mode-hook
            'julia-repl-mode))

(after! evil-magit
  (map! :mode magit-mode
        "?" #'evil-search-backward
        "K" #'evil-scroll-line-up))

(after! evil
  (evil-collection-minibuffer-setup)
  (dolist (map +default-minibuffer-maps)
    (evil-collection-define-key 'normal map (kbd "j") #'next-line)
    (evil-collection-define-key 'normal map (kbd "k") #'previous-line)
    (evil-collection-define-key 'normal map (kbd "C-n") #'next-line)
    (evil-collection-define-key 'normal map (kbd "C-p") #'previous-line)
    (evil-collection-define-key 'insert map (kbd "C-n") #'next-line)
    (evil-collection-define-key 'insert map (kbd "C-p") #'previous-line)
    (evil-collection-define-key 'insert map (kbd "C-v") #'yank)))

(load-files-in (concat doom-private-dir "mode_keybindings"))
(load-theme doom-theme)
