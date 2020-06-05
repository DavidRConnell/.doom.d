(setq doom-localleader-alt-key "C-H-a")
(setq doom-leader-alt-key "C-H-b")

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

(defvar notes-dir "~/notes/")
(defvar refs-notes (concat notes-dir  "refs/"))
(defvar refs-pdfs "~/References/")
(defvar refs-bib (concat refs-notes "master.bib"))

(after! ebib
  (setq ebib-notes-directory refs-notes)
  (setq ebib-reading-list-file (concat refs-notes "readinglist.org"))
  (setq ebib-default-directory refs-notes)
  (setq ebib-keywords-file (concat refs-notes ".keywords.txt"))
  (setq ebib-notes-show-note-method nil)
  (setq ebib-file-search-dirs
        (remove-if-not
         (lambda (f) (find-lisp-file-predicate-is-directory f refs-pdfs))
         (directory-files-recursively refs-pdfs "." 'dirs)))
  (setq ebib-file-associations '(("pdf" . "xdg-open")))

  (doom-themes-set-faces nil
    '(ebib-marked-face :foreground green))

  (map! :map 'ebib-index-mode-map
        "/" #'swiper)

  (defun dc-ebib-create-org-title (key db)
    (replace-regexp-in-string "[\t\n ]+"
                              " "
                              (or (ebib-get-field-value
                                   "title" key db 'noerror 'unbraced 'xref)
                                  "(No Title)")))

  (defun dc-ebib-create-org-author (key db)
    (replace-regexp-in-string "[\t\n ]+ "
                              " "
                              (or (ebib-get-field-value
                                   "author" key db 'noerror 'unbraced 'xref)
                                  (ebib-get-field-value
                                   "editor" key db 'noerror 'unbraced 'xref)
                                  "(No Author)")))

  (defun dc-ebib-create-org-identifier (key _)
    key)

  (setq ebib-notes-template-specifiers
        '((?K . ebib-create-org-identifier)
          (?k . dc-ebib-create-org-identifier)
          (?T . ebib-create-org-title)
          (?t . dc-ebib-create-org-title)
          (?A . dc-ebib-create-org-author)
          (?L . ebib-create-org-link)
          (?F . ebib-create-org-file-link)
          (?D . ebib-create-org-doi-link)
          (?U . ebib-create-org-url-link)
          (?M . ebib-reading-list-todo-marker)))
  (setq ebib-reading-list-template-specifiers ebib-notes-template-specifiers)

  (setq ebib-reading-list-template "* %M [[file:%k.org][%A: %t]]\n:PROPERTIES:\n:Custom_id: %k\n:END:\n")
  (setq ebib-notes-template "#+TITLE: %t\n#+AUTHOR: %A\n#+CUSTOM_ID: %k\ncite:%k\n\n>|<"))

(after! deft
  (setq deft-extensions '("org"))
  (map! :map 'deft-mode-map
        :i "C-o" #'deft-open-file-other-window
        :i "C-w" #'deft-filter-decrement-word
        :i "C-n" #'next-line
        :i "C-p" #'previous-line))

(setq-hook! 'display-line-numbers-mode-hook
  display-line-numbers 'relative)

(after! evil-matchit
  (global-evil-matchit-mode 1))

(after! company
  (global-company-mode 1))

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
  (setq evil-magit-want-horizontal-movement t)
  (map! :mode magit-mode
        "?" #'evil-search-backward
        "K" #'evil-scroll-line-up))

(after! avy
  (setq avy-keys-alist
        '((avy-goto-char . (?a ?o ?e ?u ?h ?t ?n ?s))))
  (setq avy-keys '(?a ?o ?e ?u ?h ?t ?n ?s))
  (setq avy-enter-times-out t)
  (setq avy-timeout-seconds 0.75)
  (setq aw-keys '(?a ?o ?e ?u ?h ?t ?n ?s))
  (setq avy-flyspell-correct-function #'flyspell-correct-at-point)
  (defun dc-avy-action-kill-move (pt)
    "Kill sexp at PT and move there."
    (goto-char pt)
    (avy-forward-item)
    (kill-region pt (point))
    (message "Killed: %s" (current-kill 0))
    (point)
    (evil-insert-state))

  (setq avy-dispatch-alist
        '((?c . dc-avy-action-kill-move)
          (?d . avy-action-kill-stay)
          (?g . avy-action-teleport)
          (?m . avy-action-mark)
          (?n . avy-action-copy)
          (?y . avy-action-yank)
          (?i . avy-action-ispell)
          (?z . avy-action-zap-to-char))))

(after! yasnippet
  (map! :map yas-keymap
    "C-SPC" #'yas-next-field-or-maybe-expand))

(after! langtool
  (setq langtool-java-classpath
        "/usr/share/languagetool:/usr/share/java/languagetool/*"))

(after! emacs
  (set-pretty-symbols! '(emacs-lisp-mode)
    :lambda "lambda"
    :Lambda "cmd!"))

(after! flycheck
  (flycheck-define-checker proselint
    "A linter for prose."
    :command ("proselint" source-inplace)
    :error-patterns
    ((warning line-start (file-name) ":" line ":" column ": "
              (id (one-or-more (not (any " "))))
              (message (one-or-more not-newline)
                       (zero-or-more "\n" (any " ") (one-or-more not-newline)))
              line-end))
    :modes (text-mode LaTeX-mode org-mode markdown-mode latex-mode)))

(after! (:and flycheck flyspell)
  (add-hook 'text-mode-hook
            #'flyspell-mode
            #'flycheck-mode))

(after! (:and flyspell doom-one-theme)
  (doom-themes-set-faces nil
  '(flyspell-incorrect :slant 'italic :foreground red :underline nil)
  '(flyspell-duplicate :slant 'italic :foreground yellow :underline nil)))

(after! (:and org-roam doom-one-theme)
  (doom-themes-set-faces nil
    '(org-roam-link :slant 'italic :underline t :foreground green)))

(after! (:and writegood-mode doom-one-theme)
  (doom-themes-set-faces nil
  '(writegood-weasels-face :slant 'italic :foreground blue)
  '(writegood-duplicates-face :slant 'italic :foreground orange))
  (add-hook 'writegood-mode-hook
            #'writegood-passive-voice-turn-off))

(after! evil-surround
  (add-to-list 'evil-surround-pairs-alist
               '(?< . ("< " . " >")))
  (add-to-list 'evil-surround-pairs-alist
               '(?> . ("<" . ">"))))

(after! projectile
  (setq projectile-enable-caching nil))

(add-hook! 'minibuffer-setup-hook
           #'company-mode)

(after! writeroom-mode
  (setq +zen-text-scale 0.5))

(after! counsel-org-clock
  (counsel-org-clock-rebuild-history))

(add-to-list 'auto-mode-alist '("\\.svg\\'" . image-mode))

(load-files-in (concat doom-private-dir "mode_keybindings"))
