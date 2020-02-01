(load! "+binding_functions")
(if (featurep 'evil)
    (load! "+my-evil-bindings"))

(setq +pretty-code-symbols
      '(:name "»"
              :src_block "»"
              :src_block_end "«"
              :lambda "λ"
              :Lambda "Λ"
              :def "ƒ"
              :composition "∘"
              :map "↦"
              :null "∅"
              :true "𝕋"
              :false "𝔽"
              :int "ℤ"
              :float "ℝ"
              :str "𝕊"
              :bool "𝔹"
              :not "￢"
              :in "∈"
              :not-in "∉"
              :and "∧"
              :or "∨"
              :for "∀"
              :some "∃"
              :return "⟼"
              :yield "⟻"
              :tuple "⨂"
              :pipe ""
              :dot "•"
              :not-equal "≠"
              :gt-equal "≥"
              :lt-equal "≤"))

(setq doom-font (font-spec :family "Hack Nerd Font Mono" :size 16))
(setq doom-localleader-key ",")
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
  (setq avy-enter-times-out t)
  (setq avy-timeout-seconds 1)
  (setq aw-keys '(?a ?o ?e ?u ?h ?t ?n ?s ?-)))

(after! deft
  (setq deft-directory org-notes))

(after! yasnippet
  (map! :map yas-keymap
    "C-SPC" #'yas-next-field-or-maybe-expand))

(after! langtool
  (setq langtool-java-classpath
        "/usr/share/languagetool:/usr/share/java/languagetool/*"))

(after! emacs
  (set-pretty-symbols! '(emacs-lisp-mode)
    :lambda "lambda"
    :Lambda "lambda!"))

(after! evil-surround
  (add-to-list 'evil-surround-pairs-alist
               '(?< . ("< " . " >")))
  (add-to-list 'evil-surround-pairs-alist
               '(?> . ("<" . ">"))))

(load-files-in (concat doom-private-dir "mode_keybindings"))
(load-theme doom-theme t nil)
