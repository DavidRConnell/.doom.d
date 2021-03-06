;;; +my_evil_bindings.el --- Port of my init.vim -*- lexical-binding: t; -*-
;;;

(setq evil-collection-key-blacklist
      ;;; Code:
      (list "gd" "gf" "J" "K" "[" "]" "Spc" "gR"
            doom-leader-key doom-localleader-key))


(when (featurep! :editor evil +everywhere)
  ;; Have C-u behave similarly to `doom/backward-to-bol-or-indent'.
  ;; NOTE SPC u replaces C-u as the universal argument.
  (map! :i "C-u" #'doom/backward-kill-to-bol-and-indent
        :i "C-w" #'backward-kill-word)

  ;; Minibuffer
  (define-key! evil-ex-completion-map
    "C-a"    #'move-beginning-of-line
    "C-b"    #'backward-word
    "C-h"    #'backward-char
    "C-f"    #'forward-word
    "C-l"    #'forward-char
    "M-n"    #'+company/dabbrev
    "M-p"    #'+company/dabbrev-code-previous
    [tab] (if (featurep! :completion ivy)
                #'counsel-minibuffer-history
              #'helm-minibuffer-history)
    "C-SPC" #'evil-ex-completion)

  (define-key! minibuffer-local-map
    "C-SPC" #'completion-at-point)

  (define-key! :keymaps +default-minibuffer-maps
    [escape] #'abort-recursive-edit
    "C-a"    #'move-beginning-of-line
    "C-b"    #'backward-word
    "C-h"    #'backward-char
    "C-f"    #'forward-word
    "C-l"    #'forward-char
    "C-r"    #'evil-paste-from-register
    "C-u"    #'doom/backward-kill-to-bol-and-indent
    "C-v"    #'yank
    "C-w"    #'backward-kill-word
    "C-SPC"  #'+company/complete
    ;; Scrolling lines
    "C-n"    #'next-line
    "C-p"    #'previous-line
    "C-j"    #'scroll-up-command
    "C-k"    #'scroll-down-command)

  (define-key! :keymaps '(read-expression-map minibuffer-local-map)
    "C-n" #'next-line-or-history-element
    "C-p" #'previous-line-or-history-element))

(dc--select-function-with-universal-arg
 arg-ebib-open-bibtex-file
 (cmd! (dc-goto-or-create-workspace "References")
    (ebib refs-bib))
 (cmd! (let* ((file-list
            (cl-remove-if-not
             (lambda (x) (string=
                     (file-name-extension x)
                     "bib"))
             (directory-files ".")))
           (file (cond ((eq (length file-list) 0)
                        (error "No bib files in current directory"))
                       ((eq (length file-list) 1)
                        (first file-list))
                       (t
                        (completing-read "Bib: "
                                         file-list)))))
      (dc-goto-or-create-workspace "References")
      (ebib (expand-file-name file)))))

;;
;;; Global evil keybinds
(map!
 :i [tab] (general-predicate-dispatch nil ; fall back to nearest keymap
            (and (featurep! :editor snippets)
                 (bound-and-true-p yas-minor-mode)
                 (yas-maybe-expand-abbrev-key-filter 'yas-expand))
            #'yas-expand
            (and (featurep! :completion company +tng)
                 (+company-has-completion-p))
            #'+company/complete)
 :n [tab] (general-predicate-dispatch nil
            (and (featurep! :editor fold)
                 (save-excursion (end-of-line) (invisible-p (point))))
            #'+fold/toggle
            (fboundp 'evil-jump-item)
            #'evil-jump-item)
 :v [tab] (general-predicate-dispatch nil
            (and (bound-and-true-p yas-minor-mode)
                 (or (eq evil-visual-selection 'line)
                     (not (memq (char-after) (list ?\( ?\[ ?\{ ?\} ?\] ?\))))))
            #'yas-insert-snippet
            (fboundp 'evil-jump-item)
            #'evil-jump-item)

 ;; Smarter newlines
 :i [remap newline] #'newline-and-indent  ; auto-indent on newline
 :i "C-j"           #'+default/newline    ; default behavior

 (:after help :map help-mode-map
   :ni [return]       #'link-hint-open-link-at-point)
 (:after helpful :map helpful-mode-map
   :ni [return]       #'link-hint-open-link-at-point)
 (:after info :map Info-mode-map
   :ni [return]       #'link-hint-open-link-at-point)
 (:after apropos :map apropos-mode-map
   :ni [return] #'link-hint-open-link-at-point
   :n "TAB"     #'forward-button
   :n [tab]     #'forward-button
   :n [backtab] #'backward-button)
 (:after view :map view-mode-map
   [escape]  #'View-quit-all)
 (:after man :map Man-mode-map
   :n "q"    #'kill-current-buffer)

 (:prefix "C-h"
   :desc "Find info manual" "RET" #'info-display-manual
   :desc "Open project README" "P" (cmd! (dc-project-help
                                       (concat doom-emacs-dir
                                               ".local/straight/repos/")))
   :desc "Top layer key map" "K" #'which-key-show-top-level
   :desc "External info manuals" "C-i" (cmd! (dc-find-external-manual "~/info")))

 :n   "}"       #'next-buffer
 :n   "{"       #'previous-buffer
 :n   "za"      #'spell-fu-word-add
 :n   "zr"      #'spell-fu-word-remove
 :n   "zx"      #'kill-this-buffer
 :n   "ZX"      #'bury-buffer
 :n   "gp"      #'+evil/reselect-paste
 :n   "g="      #'widen
 :v   "g="      #'+evil:narrow-buffer
 :nv  "z="      #'flyspell-correct-at-point
 :nv  "g@"      #'+evil:apply-macro
 :nv  "Q"       #'+evil:apply-macro
 :nv  "C-q"     #'evil-execute-macro
 :nv  "gx"      #'evil-exchange
 :nv  "gK"      #'+lookup/documentation
 :nv  "gc"      #'evilnc-comment-operator
 :nv  "gr"      #'dc-apply-ex-substitute-word
 :nv  "gR"      #'dc-apply-ex-substitute-WORD
 :nv  "gk"      #'dc-apply-ex-substitute-custom
 :v   "@"       #'+evil:apply-macro
 :v   "."       #'+evil:apply-macro
 :v   "<"       #'+evil/visual-dedent
 :v   ">"       #'+evil/visual-indent
 :nv  "j"       #'evil-next-visual-line
 :nv  "gj"      #'join-line
 :nv  "k"       #'evil-previous-visual-line
 :nv  "H"       #'evil-first-non-blank
 :nv  "L"       #'evil-end-of-line
 :nv  "K"       #'evil-scroll-line-up
 :nv  "J"       #'evil-scroll-line-down
 :nv  "C-j"     #'avy-goto-line-below
 :nv  "C-k"     #'avy-goto-line-above
 :nv  "C-'"     #'counsel-evil-registers
 :nv  "C-m"     #'evil-goto-mark
 :nv  "M"       #'counsel-evil-marks
 :nv  "ZZ"      #'save-buffers-kill-terminal
 :nv  "/"       (dc-arg-cmd #'swiper-isearch #'swiper)
 :nv  "?"       (dc-arg-cmd #'swiper-isearch-backward
                            #'swiper-backward)
 :nv  "gn"      (dc-arg-cmd #'swiper-isearch-thing-at-point
                            #'swiper-thing-at-point)
 :nvi "M-/"     #'link-hint-open-link
 :nvi "M-?"     #'link-hint-copy-link
 :nv  "C-u"     #'universal-argument
 :i   "C-u"     #'doom/backward-kill-to-bol-and-indent
 :nv  "U"       #'undo-tree-visualize
 :nvi "C-c C-r" (cond ((featurep! :completion ivy) #'ivy-resume)
                      ((featurep! :completion helm) #'helm-resume))
 :nvm "C-n"     #'evil-next-line
 :nvm "C-p"     #'evil-previous-line
 :i   "C-e"     #'end-of-line

 ;; Use C-s (snipe) instead of C-t because C-t is my stumpwm key
 :nvm "C-s" (cmd! ; make exclusive (more like till than from)
             (call-interactively #'avy-goto-char-in-line)
             (point))

 :nvm "C-f" #'avy-goto-char-in-line
 :nvm "C-/" #'avy-goto-char-2

 (:after evil-easymotion
   :m   "C-e" evilem-map

   :o "j"   #'evilem-motion-next-line
   :o "k"   #'evilem-motion-previous-line
   :o "C-w" #'evilem-motion-forward-word-begin
   :o "C-e" #'evilem-motion-forward-word-end
   :o "C-b" #'evilem-motion-backward-word-begin)

 (:after org
   :map org-mode-map
   :prefix "<easymotion>"
   "h" #'+org/goto-visible)

 (:after gud
   :nv "M-u" #'gud-up
   :nv "M-d" #'gud-down
   :nv "M-b" #'gud-break
   :nv "M-r" #'gud-remove
   :nv "M-s" #'gud-step
   :nv "M-n" #'gud-next
   :nv "M-c" #'gud-cont
   :nv "M-S" #'gud-show-stack
   :nv "M-l" #'gud-list-breakpoints)

 (:when (featurep! :ui workspaces)
   :nv "M-t"   #'+workspace/new
   :nv "M-T"   #'+workspace/display
   :nv "M-q"   #'+workspace/delete
   (:prefix "C-c"
    (:prefix ("C-w" . "Workspaces")
     "h"   #'+workspace/switch-left
     "l"   #'+workspace/switch-right
     "r"   #'+workspace/rename
     "o"   #'+workspace/other
     "g"   #'+workspace/switch-to))
   :nv "M-1"   (λ! (+workspace/switch-to 0))
   :nv "M-2"   (λ! (+workspace/switch-to 1))
   :nv "M-3"   (λ! (+workspace/switch-to 2))
   :nv "M-4"   (λ! (+workspace/switch-to 3))
   :nv "M-5"   (λ! (+workspace/switch-to 4))
   :nv "M-6"   (λ! (+workspace/switch-to 5))
   :nv "M-7"   (λ! (+workspace/switch-to 6))
   :nv "M-8"   (λ! (+workspace/switch-to 7))
   :nv "M-9"   (λ! (+workspace/switch-to 8))
   :nv "M-0"   #'+workspace/switch-to-final)

 :n "C-w =" #'balance-windows-area)

(map! (:when (featurep! :completion company)
        :i "C-SPC"    #'+company/complete
        :i "C-n"      #'+company/dabbrev
        :i "C-f"      #'company-files
        :i "C-k"      #'+company/dict-or-keywords
        :i "C-\\"     #'company-math-symbols-unicode
        (:prefix "C-x"
          :i "C-l"    #'+company/whole-lines
          :i "C-k"    #'+company/dict-or-keywords
          :i "C-f"    #'company-files
          :i "C-]"    #'company-etags
          :i "s"      #'company-ispell
          :i "C-s"    #'company-yasnippet
          :i "C-o"    #'company-capf
          :i "C-n"    #'+company/dabbrev
          :i "C-p"    #'+company/dabbrev-code-previous
          :i "C-u"    #'company-math-symbols-unicode)
        (:after company
          (:map company-active-map
            "C-w"     nil  ; don't interfere with `evil-delete-backward-word'
            "C-n"     #'company-select-next
            "C-p"     #'company-select-previous
            "C-h"     #'company-show-doc-buffer
            "C-s"     #'company-filter-candidates
            "C-S-s"   (cond ((featurep! :completion helm) #'helm-company)
                            ((featurep! :completion ivy)  #'counsel-company))
            "C-SPC"   #'company-complete-common
            "C-i"     #'company-complete-common-or-cycle
            [tab]     #'company-complete-common-or-cycle
            [backtab] #'company-select-previous)
          (:map company-search-map  ; applies to `company-filter-map' too
            "C-n"     #'company-select-next-or-abort
            "C-p"     #'company-select-previous-or-abort
            "C-s"     (λ! (company-search-abort) (company-filter-candidates))
            "ESC"     #'company-search-abort)
          ;; TAB auto-completion in term buffers
          (:map comint-mode-map
            "C-SPC" #'company-complete)))

      (:when (featurep! :completion ivy)
        (:after ivy
          :map ivy-minibuffer-map
          "C-SPC"   #'ivy-partial-or-done ; preview file
          "C-j"     #'ivy-alt-done
          "C-k"     nil
          "C-r"     #'counsel-minibuffer-history
          "C-v"     #'yank
          "C-s"     #'ivy-mark
          "C--"     #'ivy-unmark
          "C-c C-m" #'ivy-toggle-marks
          "C-c C-i" #'ivy-toggle-ignore
          "C-c C-f" #'ivy-toggle-fuzzy
          "C-c C-a" #'ivy-toggle-calling)
        (:after counsel
          :map shell-mode-map
          "C-r"   #'counsel-shell-history
          :map counsel-ag-map
          "C-SPC"    #'ivy-partial-or-done ; preview
          "C-j"      #'ivy-done
          "C-c C-e"  #'+ivy/woccur      ; search/replace on results
          [backtab]  #'+ivy/woccur      ; search/replace on results
          [C-return] #'+ivy/git-grep-other-window-action)
        (:after swiper
          :map swiper-map
          [backtab] #'+ivy/wgrep-occur
          [C-return] #'+ivy/git-grep-other-window-action))

	  (:when (featurep! :completion helm)
			 (:after helm :map helm-map
					 [remap next-line]     #'helm-next-line
					 [remap previous-line] #'helm-previous-line
					 [left]     #'left-char
					 [right]    #'right-char
					 "C-S-f"    #'helm-previous-page
					 "C-S-n"    #'helm-next-source
					 "C-S-p"    #'helm-previous-source
					 (:when (featurep! :editor evil +everywhere)
							"C-j"    #'helm-next-line
							"C-k"    #'helm-previous-line
							"C-S-j"  #'helm-next-source
							"C-S-k"  #'helm-previous-source)
					 "C-u"      #'helm-delete-minibuffer-contents
					 "C-s"      #'helm-minibuffer-history
					 ;; Swap TAB and C-z
					 "TAB"      #'helm-execute-persistent-action
					 [tab]      #'helm-execute-persistent-action
					 "C-z"      #'helm-select-action)
       (:after helm-ag :map helm-ag-map
			   "C--"      #'+helm-do-ag-decrease-context
        "C-="      #'+helm-do-ag-increase-context
        [left]     nil
        [right]    nil)
       (:after helm-files :map (helm-find-files-map helm-read-file-map)
        [C-return] #'helm-ff-run-switch-other-window
        "C-w"      #'helm-find-files-up-one-level)
       (:after helm-locate :map helm-generic-files-map
        [C-return] #'helm-ff-run-switch-other-window)
       (:after helm-buffers :map helm-buffer-map
        [C-return] #'helm-buffer-switch-other-window)
       (:after helm-occur :map helm-occur-map
        [C-return] #'helm-occur-run-goto-line-ow)
       (:after helm-grep :map helm-grep-map
        [C-return] #'helm-grep-run-other-window-action)))

(map! (:when (featurep! :editor fold)
        :nv "C-SPC" #'+fold/toggle)
      (:when (featurep! :editor format)
        :n "gQ" #'+format:region)
      (:when (featurep! :editor snippets)
        :i  "C-a" #'aya-expand
        :nv "C-a" #'aya-create
        :i  "C-s" #'yas-expand))

(after! vterm
  (add-hook! 'vterm-mode-hook
             #'evil-emacs-state)

  (map! :map 'vterm-mode-map
        :n "a" (cmd! (evil-emacs-state)
                  (vterm-send-key (kbd "C-["))
                  (vterm-send-key (kbd "l"))
                  (vterm-send-key (kbd "a")))
        :n "i" (cmd! (evil-emacs-state)
                  (vterm-send-key (kbd "C-["))
                  (vterm-send-key (kbd "l"))
                  (vterm-send-key (kbd "i")))
        :e "C-[" (cmd! (vterm-send-key (kbd "C-[")))
        :nie "C-d" #'vterm-send-C-d
        :e "C-g" #'evil-normal-state
        (:localleader
         "j" #'multi-vterm-next
         "k" #'multi-vterm-prev
         "n" #'multi-vterm
         "t" #'multi-vterm-dedicated-toggle
         "p" #'multi-vterm-projectile)))

(map! :leader
      "w"    #'save-buffer
      "q"    #'kill-this-buffer
      "o"    #'find-file
      "O"    #'counsel-locate

      "b"    (dc-arg-cmd #'+ivy/switch-workspace-buffer
                         #'dc-workspace/switch-to)

      "B"    #'switch-to-buffer
      "+"    #'evil-numbers/inc-at-pt
      "-"    #'evil-numbers/dec-at-pt
      ","    (dc-arg-cmd #'vterm
                         #'+vterm/toggle
                         #'+vterm/here)
      "<"    #'counsel-switch-to-shell-buffer

      :desc "journal" "j"    (cmd! (dc-run-deft-in-workspace
                                 "Journal" "~/journal/"))

      "s"    #'doom/open-scratch-buffer
      :desc "Find file on server" "S" #'dc-find-file-on-server
      "!"    #'doom/sudo-this-file
      ":"    #'helm-eval-expression
      ";"    #'execute-extended-command
      :desc ".doom.d" "."    (cmd! (dc-open-in-workspace "Doom" "~/.doom.d"))
      "m"    #'+popup/toggle
      "/"    #'evil-ex-nohighlight
      "\\"   #'toggle-truncate-lines
      "e"    (cmd! (dc-goto-or-create-workspace "elfeed")
                (elfeed))

      :desc "Find file in project"  "SPC"  #'projectile-find-file
      :desc "Search for symbol in project" "*" #'+default/search-project-for-symbol-at-point
      :desc "show functions" "l" #'imenu

      :desc "Surrond With" "["    (cmd! (sp-rewrap-sexp (dc--get-sp-pair "[")))
      :desc "Surrond With" "{"    (cmd! (sp-rewrap-sexp (dc--get-sp-pair "{")))
      :desc "Surrond With" "("    (cmd! (sp-rewrap-sexp (dc--get-sp-pair "(")))
      :desc "Surrond With" "\""   (cmd! (sp-rewrap-sexp (dc--get-sp-pair "\"")))
      :desc "Open Notes"   "N"    (cmd! (dc-open-in-workspace "Notes" notes-dir))

      (:prefix ("r" . "Replace line")
        :desc "custom" "c" (cmd!
                            (evil-ex
                             (concat (dc--get-evil-ex-prefix) "s/")))

        :desc "word" "w" (cmd!
                          (evil-ex
                           (concat
                            (dc--get-evil-ex-prefix)
                            "s/\\<" (thing-at-point 'word) "\\>/")))

        :desc "WORD" "W" (cmd!
                          (evil-ex
                           (concat
                            (dc--get-evil-ex-prefix)
                            "s/\\<" (thing-at-point 'symbol) "\\>/"))))

      (:prefix ("R" . "Replace buffer")
        :desc "custom" "c" (cmd! (evil-ex "%s/"))
        :desc "word" "w" (cmd!
                          (evil-ex
                           (concat "%s/\\<" (thing-at-point 'word) "\\>/")))

        :desc "WORD" "W" (cmd!
                          (evil-ex
                           (concat "%s/\\<" (thing-at-point 'symbol) "\\>/"))))

      (:prefix ("p" . "Projects")
        :desc "open emacs.d" "e" (cmd! (dc-open-in-workspace "Emacs" doom-emacs-dir))
        :desc "open config" "c" (cmd! (dc-open-in-workspace "Config" doom-private-dir))
        "R" #'projectile-replace
        "g" (cond ((featurep! :completion ivy) #'counsel-projectile-grep)
                  ((featurep! :completion helm) #'+helm/project-search))
        "o" #'projectile-switch-project
        "f" #'projectile-find-file
        "r" #'counsel-buffer-or-recentf)

      (:prefix ("c" . "Comments")
        "l"    #'evilnc-comment-or-uncomment-lines)

      "G" #'magit-dispatch
      (:prefix ("g" . "git")
        :desc "Revert" "R" #'vc-revert
        (:when (featurep! :ui vc-gutter)
          :desc "Git revert hunk"           "r"   #'git-gutter:revert-hunk
          :desc "Git stage hunk"            "h"   #'git-gutter:stage-hunk
          :desc "Git time machine"          "t"   #'git-timemachine-toggle
          :desc "Jump to next hunk"         "n"   #'git-gutter:next-hunk
          :desc "Jump to previous hunk"     "N"   #'git-gutter:previous-hunk)
        (:when (featurep! :tools magit)
          :desc "status"        "s" #'magit-status
          :desc "add"           "a" #'magit-stage-file
          :desc "add all"       "A" #'magit-stage-modified
          :desc "unstage"       "u" #'magit-unstage-file
          :desc "file log"      "l" #'magit-log-buffer-file
          :desc "branch log"    "L" #'magit-log-current
          :desc "commit"        "c" #'magit-commit-create
          :desc "diff"          "d" #'magit-diff-buffer-file
          :desc "diff worktree" "D" #'magit-diff-working-tree
          :desc "checkout"      "b" #'magit-branch-or-checkout
          :desc "Browse remote" "B" #'forge-browse-remote
          (:prefix ("S" . "stash")
            :desc "pop"    "P" #'magit-stash-pop
            :desc "push"   "p" #'magit-stash-worktree
            :desc "list"   "l" #'magit-stash-list
            :desc "show"   "s" #'magit-stash-show
            :desc "drop"   "d" #'magit-stash-drop
            :desc "branch" "b" #'magit-stash-branch)))

      (:prefix ("M" . "Mail")
        :desc "Go to mail"   "g" #'=mu4e
        :desc "Compose mail" "c" #'mu4e-compose-new
        :desc "Open draft"   "d" (cmd! (counsel-find-file (concat
                                                        mu4e-maildir
                                                        "gmail/drafts/cur"))))

      (:prefix ("C" . "Code checking")
        :desc "toggle spell"     "s" #'flyspell-mode
        :desc "Check strings"    "S" #'ispell-comments-and-strings
        :desc "toggle lint"      "c" #'flycheck-mode
        :desc "toggle langcheck" "g" #'langtool-check
        :desc "list errors"      "l" #'counsel-flycheck)

      (:prefix ("t" . "Todo")
        :desc "Agenda"         "a"  #'org-agenda
        :desc "Todo list"      "t"  #'org-todo-list
        :desc "Tags search"    "m"  #'org-tags-view
        :desc "Org capture"    "x"  #'counsel-org-capture
        :desc "Go to org file" "g"  (cmd! (dc-open-org-file-in-workspace
                                        "Org" org-directory))
        :desc "Org store link" "l"  #'org-store-link
        :desc "View search"    "v"  #'org-search-view
        :desc "Deft"           "d"  (cmd! (dc-run-deft-in-workspace
                                        "Org" org-directory))
        (:prefix ("c" . "Clock")
          :desc "Pomodoro timer" "p" #'org-pomodoro
          :desc "Clock in"       "i" #'counsel-org-clock-context
          :desc "Clock out"      "o" #'org-clock-out
          :desc "Clock in last"  "I" #'org-clock-in-last
          :desc "Cancel"         "c" #'org-clock-cancel
          :desc "Goto last"      "g" #'counsel-org-clock-goto))

      (:prefix ("n" . "Reference Notes")
       :desc "Ebib"              "e" #'arg-ebib-open-bibtex-file
       :desc "bibtex"            "b"
       (cond ((featurep! :completion ivy) #'ivy-bibtex)
             ((featurep! :completion helm) #'helm-bibtex))
       :desc "Master bib file"   "m" (cmd! (dc-open-in-workspace
                                         "References" refs-bib))
       :desc "Open Reference"    "r" #'ivy-bibtex
       :desc "Go to notes"       "g" (cmd! (dc-open-org-file-in-workspace
                                         "References" refs-notes))
       :desc "Open reading list" "l" (cmd! (dc-open-in-workspace
                                         "References"
                                         (concat refs-notes "readinglist.org")))
       :desc "Deft"              "d" (cmd! (dc-run-deft-in-workspace
                                         "References" refs-notes)))

      (:prefix ("z" . "Zettle")
       :desc "Go to zettle"      "g" (cmd! (dc-goto-or-create-workspace "Zettle")
                                        (org-roam-find-file))
       :desc "Capture"           "x" #'org-roam-capture
       :desc "Deft"              "d" (cmd! (dc-run-deft-in-workspace
                                         "Zettle" zettle-dir))
       :desc "Go to today"       "t" (cmd! (dc-open-in-workspace
                                         "Zettle"
                                         (concat zettle-dir "today.org")))
       :desc "Go to next"        "n" (cmd! (dc-open-in-workspace
                                         "Zettle"
                                         (concat zettle-dir "next.org")))
       :desc "Go to index"       "i" (cmd! (dc-open-in-workspace
                                         "Zettle"
                                         (concat zettle-dir "index.org"))))

      (:prefix ("d" . "Define")
       :desc "Define word"      "D" (dc-arg-cmd (cmd! (sdcv-search (word-at-point)))
                                                #'sdcv-search)
       :desc "MW Define word"   "d" #'mw-thesaurus-lookup-at-point
       :desc "Thesaurus"        "t" (dc-arg-cmd (cmd! (synosaurus-lookup (word-at-point)))
                                                #'synosaurus-lookup)
       :desc "Wordnut"          "w" (dc-arg-cmd #'wordnut-lookup-current-word
                                                #'wordnut-search)
       :desc "Wiki summary"     "k" (dc-arg-cmd (cmd! (wiki-summary (word-at-point)))
                                                #'wiki-summary)
       :desc "Biblio lookup"    "b" #'biblio-lookup
       :desc "Add bib entry"    "c" #'doi-utils-add-entry-from-crossref-query
       :desc "Search duck"      "s" #'counsel-search))
