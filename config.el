(load! "+binding_functions")
(if (featurep 'evil)
    (load! "+my-evil-bindings"))

(setq doom-font (font-spec :family "Hack Nerd Font Mono" :size 16))
(setq doom-localleader-key ",")
(setq doom-theme 'doom-one)
(setq doom-scratch-buffer-major-mode 'emacs-lisp-mode)

(defvar org-notes "~/notes/")
(defvar refs-pdfs "~/References/")
(defvar refs-bib (concat refs-pdfs "index.bib"))
(defvar refs-notes (concat org-notes "refs.org"))

(setq-hook! 'display-line-numbers-mode-hook
  display-line-numbers 'relative)

(defun load-files-in (dir)
  "Load all files under directory DIR."
  (setq file-names (directory-files dir))
  (while file-names
    (if (string-suffix-p ".el" (car file-names))
        (load! (string-join (list dir (car file-names)) "/")))
    (setq file-names (cdr file-names))))

(load-files-in (concat doom-private-dir "mode_configs"))

(after! git-timemachine
  (add-hook 'git-timemachine-mode-hook
            #'git-timemachine-show-revision-fuzzy)
  (map! :mode git-timemachine-mode
        "C-p" #'git-timemachine-show-previous-revision
        "C-n" #'git-timemachine-show-next-revision))

(after! evil-magit
  (map! :mode magit-mode
        "?" #'evil-search-backward
        "K" #'evil-scroll-line-up
        "L" #'evil-last-non-blank
        "H" #'evil-first-non-blank
        "l" #'evil-forward-char
        "h" #'evil-backward-char))

(after! avy
  (setq avy-keys-alist
        '((avy-goto-char . (?a ?o ?e ?u ?h ?t ?n ?s ?-))))
  (setq avy-keys (number-sequence ?a ?z))
  (setq avy-enter-times-out nil)
  (setq avy-timeout-seconds 0.3))

(after! yasnippet
  (map! :map yas-keymap
    "C-SPC" #'yas-next-field-or-maybe-expand))

(load-files-in (concat doom-private-dir "mode_keybindings"))
(load-theme doom-theme)
