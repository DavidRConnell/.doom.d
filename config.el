(setq default-fill-column 80)
(defun load-files-in (dir)
  "Load all files under directory DIR."
  (setq file-names (directory-files dir))
  (while file-names
    (if (string-suffix-p ".el" (car file-names))
        (load! (string-join (list dir (car file-names)) "/")))
    (setq file-names (cdr file-names))))

(defvar org-notes "~/notes/")
(defvar refs-pdfs "~/Documents/references/")
(defvar refs-bib (concat refs-pdfs "index.bib"))
(defvar refs-notes (concat org-notes "refs.org"))

(if (featurep 'evil)
    (load! "+my-evil-bindings"))

(use-package! org-ref
  :config
  (setq org-ref-completion-library 'org-ref-ivy-cite))

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
    (load (concat doom-private-dir "mode_configs/+org.el"))))

(add-hook! 'org-agenda-mode-hook
  (load (concat doom-private-dir "mode_keybindings/+org.el")))

;; Reload theme to poorly fix cursor color.
(load-theme 'doom-one)
