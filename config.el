(setq default-fill-column 80)
(defun load-files-in (dir)
  "Load all files under directory DIR."
  (setq file-names (directory-files dir))
  (while file-names
    (if (string-suffix-p ".el" (car file-names))
        (load! (string-join (list dir (car file-names)) "/")))
    (setq file-names (cdr file-names))))

(if (featurep 'evil)
    (load! "+my-evil-bindings"))

(load-files-in (concat doom-private-dir "mode_configs"))
(load-files-in (concat doom-private-dir "mode_keybindings"))

(add-hook 'git-timemachine-mode-hook
          #'git-timemachine-show-revision-fuzzy
          (map! :mode git-timemachine-mode
                :ni "C-p" #'git-timemachine-show-previous-revision
                :ni "C-n" #'git-timemachine-show-next-revision))

(add-hook 'term-mode-hook
          #'company-mode)

(add-hook 'matlab-shell-mode-hook
          #'company-mode)

(add-hook 'julia-mode-hook
          'julia-repl-mode)

(after! evil-magit
  (map! :mode magit-mode
        "?" #'evil-search-backward
        "K" #'evil-scroll-line-up))

(after! org
  (progn
    (delete '("\\.pdf\\'" . default) org-file-apps)
    (add-to-list 'org-file-apps '("pdf" . "zathura %s"))
    (load (concat doom-private-dir "mode_configs/+org.el"))))

(add-hook! 'org-agenda-mode-hook
  (load (concat doom-private-dir "mode_keybindings/+org.el")))

;; Reload theme to poorly fix cursor color.
(load-theme 'doom-one)
