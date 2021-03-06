(add-to-list 'load-path (concat doom-private-dir "extras"))

(defvar notes-dir "~/notes/")
(defvar zettle-dir (concat notes-dir "zettle/"))
(defvar refs-notes (concat notes-dir  "refs/"))
(defvar refs-pdfs "~/References/")
(defvar refs-bib (concat refs-notes "master.bib"))

(add-hook 'emacs-startup-hook (cmd! (find-file
                                  (concat zettle-dir "today.org"))
                                 (persp-rename "Zettle")))

(setq doom-localleader-alt-key "C-H-a")
(setq doom-leader-alt-key "C-H-b")

(setq user-full-name "David R. Connell")

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
(setq doom-variable-pitch-font (font-spec :family "ETBembo" :size 18))
(setq doom-localleader-key ",")
(setq doom-theme 'modus-operandi)
(setq doom-scratch-buffer-major-mode 'emacs-lisp-mode)

(use-package! ebib
  :commands ebib
  :config
  (setq ebib-notes-directory refs-notes
        ebib-reading-list-file (concat refs-notes "readinglist.org")
        ebib-default-directory refs-notes
        ebib-keywords-file (concat refs-notes ".keywords.txt")
        ebib-notes-show-note-method nil
        ebib-file-associations '(("pdf" . "xdg-open")))

  (setq ebib-file-search-dirs
        (cl-remove-if-not
         (lambda (f) (find-lisp-file-predicate-is-directory f refs-pdfs))
         (directory-files-recursively refs-pdfs "." 'dirs)))

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

  (setq ebib-reading-list-template "* %M [[file:%k.org][%t]]\n:PROPERTIES:\n:AUTHOR: %A\n:END:\ncite:%k\n\n")
  (setq ebib-notes-template "#+TITLE: %t\n#+AUTHOR: %A\ncite:%k\n\n>|<"))

(after! deft
  (setq deft-extensions '("org"))
  (setq deft-new-file-format "%Y%m%d%H%M%S")
  (setq deft-use-filter-string-for-filename nil)
  (map! :map 'deft-mode-map
        :i "C-o" #'deft-open-file-other-window
        :i "C-w" #'deft-filter-decrement-word
        :i "C-n" #'next-line
        :i "C-p" #'previous-line))


(setq-hook! 'display-line-numbers-mode-hook
  display-line-numbers 'nil)

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
  (map! :mode magit-mode
        "?" #'evil-search-backward
        "K" #'evil-scroll-line-up
        :n "gr" #'magit-refresh))

(after! ace-window
  (setq aw-keys '(?u ?h ?e ?t ?o ?n ?a ?s)))

(after! avy
  (setq avy-keys-alist
        '((avy-goto-char . (?u ?h ?e ?t ?o ?n ?a ?s))))
  (setq avy-keys '(?u ?h ?e ?t ?o ?n ?a ?s))
  (setq avy-enter-times-out t)
  (setq avy-timeout-seconds 0.75)
  (setq aw-keys '(?u ?h ?e ?t ?o ?n ?a ?s))
  (setq avy-flyspell-correct-function #'flyspell-correct-at-point)
  (defun dc-avy-action-kill-move (pt)
    "Kill sexp at PT and move there."
    (goto-char pt)
    (avy-forward-item)
    (kill-region pt (point))
    (message "Killed: %s" (current-kill 0))
    (point)
    (evil-insert-state))

  (defun dc-avy-action-add-word-to-dictionary (pt)
    "Kill sexp at PT and move there."
    (save-excursion
      (goto-char pt)
      (spell-fu-word-add)
      (message "Saved to dictionary: %s" (word-at-point))))

  (setq avy-dispatch-alist
        '((?c . dc-avy-action-kill-move)
          (?d . avy-action-kill-stay)
          (?g . avy-action-teleport)
          (?m . avy-action-mark)
          (?n . avy-action-copy)
          (?y . avy-action-yank)
          (?i . avy-action-ispell)
          (?I . dc-avy-action-add-word-to-dictionary)
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
  (setq flycheck-textlint-config "~/.textlintrc"
        flycheck-textlint-executable "~/node_modules/textlint/bin/textlint.js")
  (setq flycheck-checkers (remove 'proselint flycheck-checkers)))

(after! (:and flycheck flyspell)
  (add-hook 'text-mode-hook
            #'flycheck-mode))

(require '+colors)
(after! flyspell
  (doom-themes-set-faces nil
    '(flyspell-duplicate :slant 'italic :foreground yellow :underline nil)
    '(flyspell-incorrect :slant 'italic :foreground red :underline nil)))

(after! spell-fu
  (doom-themes-set-faces nil
    '(spell-fu-incorrect-face :slant 'italic :foreground red)))

(after! ispell
  (setq ispell-personal-dictionary "~/.aspell.en.pws"))

(after! org-roam
  (doom-themes-set-faces nil
    '(org-roam-link :slant 'italic :underline nil :foreground green)))

(after! writegood-mode
  (doom-themes-set-faces nil
  '(writegood-weasels-face :slant 'italic :foreground blue)
  '(writegood-duplicates-face :slant 'italic :foreground orange))
  (add-hook 'writegood-mode-hook
            #'writegood-passive-voice-turn-off))

(use-package! smartparens
  :config
  (setq smartparens-strict-mode t))

(use-package! evil-smartparens
  :config (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode))

(after! evil-surround
  (add-to-list 'evil-surround-pairs-alist
               '(?< . ("< " . " >")))
  (add-to-list 'evil-surround-pairs-alist
               '(?> . ("<" . ">"))))

(after! projectile
  (setq projectile-enable-caching nil))

(after! writeroom-mode
  (setq +zen-text-scale 0.5))

(after! counsel-org-clock
  (counsel-org-clock-rebuild-history))

(after! org-roam
  (setq org-roam-index-file "index.org")
  (setq org-roam-buffer-height 0.5)
  (setq org-roam-buffer-position 'bottom))

(after! (:and org-ref link-hint)
  (defun link-hint--next-org-ref-cite (&optional bound)
    "Find the next org-ref citation.
Only search the range between just after the point and BOUND."
    (link-hint--next-regexp org-ref-cite-re bound))

  (defun link-hint--org-ref-cite-at-point-p ()
    "Return the org-ref citation at the point or nil."
    (let ((sym (format "%s" (symbol-at-point))))
      (if (string-match-p org-ref-cite-re sym)
          sym
        nil)))

  (link-hint-define-type 'org-ref-cite
    :next #'link-hint--next-org-ref-cite
    :at-point-p #'link-hint--org-ref-cite-at-point-p
    :open #'org-ref-open-notes-at-point
    :copy #'org-ref-open-pdf-at-point)

  (push 'link-hint-org-ref-cite
        link-hint-types))

(use-package! nov
  :mode ("\\epub\\'" . nov-mode)
  :config
  (add-hook 'nov-mode-hook #'variable-pitch-mode)
  (setq nov-text-width 60))

(after! ess
  (map! :map 'ess-r-mode-map
        (:prefix "C-c"
         "C-a" (cmd! (insert " <- "))))
  (set-popup-rule! "\*R.*\*" :ignore t))

(use-package! mu4e
  :mode ("\\DS\\'" . mu4e-compose-mode))

(load-files-in (concat doom-private-dir "mode_keybindings"))
(put 'upcase-region 'disabled nil)

(use-package! evil-lisp-state
  :config
  (evil-lisp-state-leader "")
  (add-to-list 'evil-lisp-state-major-modes 'common-lisp-mode)
  (setq evil-lisp-state-cursor '(bar +evil-emacs-cursor-fn))
  (add-to-list 'doom-evil-state-alist '(?l . lisp)))

(use-package! iedit
  :config
  (use-package! evil-iedit-state
    :config (add-hook 'iedit-mode-hook 'evil-iedit-state)))

(use-package! eww
  :config
  (setq browse-url-browser-function 'eww-browse-url
        url-cookie-trusted-urls '()
        url-cookie-untrusted-urls '(".*")))

(use-package! org-roam-server
  :after org-roam
  :config
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 8078
        org-roam-server-export-inline-images t))
