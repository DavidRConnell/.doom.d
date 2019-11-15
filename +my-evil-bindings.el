;;; +my_evil_bindings.el --- Port of my init.vim -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;;             Things TODO main keybindings
;;;                         Stop auto delete (smart-parens)

(setq evil-collection-key-blacklist
      ;;; Code:
      (list "C-j" "C-k" "gd" "gf" "J" "K" "[" "]" "gz" "Spc"
            doom-leader-key doom-localleader-key
            doom-leader-alt-key doom-localleader-alt-key))

(map!
        ;; Global evil keybinds
        :m  "]a"    #'evil-forward-arg
        :m  "[a"    #'evil-backward-arg
        :m  "]o"    #'outline-next-visible-heading
        :m  "[o"    #'outline-previous-visible-heading
        :n  "}"     #'next-buffer
        :n  "{"     #'previous-buffer
        :n  "zx"    #'kill-this-buffer
        :n  "ZX"    #'bury-buffer
        :n  "gp"    #'+evil/reselect-paste
        :n  "g="    #'widen
        :v  "g="    #'+evil:narrow-buffer
        :nv "z="    #'flyspell-correct-word-generic
        :nv "g@"    #'+evil:apply-macro
        :nv "gx"    #'evil-exchange
        :nv "gd"    #'dumb-jump-go
        :nv "C-a"   #'evil-numbers/inc-at-pt
        :nv "C-S-a" #'evil-numbers/dec-at-pt
        :v  "gp"    #'+evil/paste-preserve-register
        :v  "@"     #'+evil:apply-macro
        :v  "."     #'+evil:apply-macro
        :v  "<"     #'+evil/visual-dedent
        :v  ">"     #'+evil/visual-indent
        :nv "j"     #'evil-next-visual-line
        :nv "k"     #'evil-previous-visual-line
        :nv "H"     #'evil-first-non-blank
        :nv "L"     #'evil-end-of-line
        :nv "K"     #'evil-scroll-line-up
        :nv "J"     #'evil-scroll-line-down
        :nv "ZZ"    #'save-buffers-kill-terminal
        :nv "/" #'swiper
        :nv "?" #'swiper-backward
        :nv "gn" #'swiper-thing-at-point

        (:prefix "C-h"
          :desc "Find info manual" "RET" #'info-display-manual
          :desc "Top layer key map" "K" #'which-key-show-top-level)

        ;; Smarter newlines
        :i [remap newline] #'newline-and-indent  ; auto-indent on newline
        :i "C-j"           #'+default/newline    ; default behavior

        :m "r" #'avy-goto-char-in-line
        :m "R" #'avy-goto-char-2

        :m  "gs"    #'+evil/easymotion  ; lazy-load `evil-easymotion'
        (:after evil-easymotion
          :map evilem-map
          "SPC" #'avy-goto-char-timer
          "/" (evilem-create #'evil-ex-search-next
                             :pre-hook (save-excursion (call-interactively #'swiper))
                             :bind ((evil-search-wrap)))
          "?" (evilem-create #'evil-ex-search-previous
                             :pre-hook (save-excursion (call-interactively #'swiper-backward))
                             :bind ((evil-search-wrap))))

        (:when (featurep! :ui workspaces)
          :nv "M-t"   #'+workspace/new
          :nv "M-T"   #'+workspace/display
          :nv "M-q"    #'+workspace/delete
          :nv "M-1"   (λ! (+workspace/switch-to 0))
          :nv "M-2"   (λ! (+workspace/switch-to 1))
          :nv "M-3"   (λ! (+workspace/switch-to 2))
          :nv "M-4"   (λ! (+workspace/switch-to 3))
          :nv "M-5"   (λ! (+workspace/switch-to 4))
          :nv "M-6"   (λ! (+workspace/switch-to 5))
          :nv "M-7"   (λ! (+workspace/switch-to 6))
          :nv "M-8"   (λ! (+workspace/switch-to 7))
          :nv "M-9"   (λ! (+workspace/switch-to 8))
          :nv "M-0"   #'+workspace/switch-to-last))

(map! (:when (featurep! :completion company)
        :i "C-SPC"    #'+company/complete
        :i "C-n"      #'+company/dabbrev
        :i "C-f"      #'company-files
        :i "C-k"      #'+company/dict-or-keywords
        (:prefix "C-x"
          :i "C-l"    #'+company/whole-lines
          :i "C-k"    #'+company/dict-or-keywords
          :i "C-f"    #'company-files
          :i "C-]"    #'company-etags
          :i "s"      #'company-ispell
          :i "C-s"    #'company-yasnippet
          :i "C-o"    #'company-capf
          :i "C-n"    #'+company/dabbrev
          :i "C-p"    #'+company/dabbrev-code-previous)
        (:after company
          (:map company-active-map
            "C-w"     nil  ; don't interfere with `evil-delete-backward-word'
            "C-n"     #'company-select-next
            "C-p"     #'company-select-previous
            "C-j"     #'company-select-next
            "C-k"     #'company-select-previous
            "C-h"     #'company-show-doc-buffer
            "C-u"     #'company-previous-page
            "C-d"     #'company-next-page
            "C-s"     #'company-filter-candidates
            "C-S-s"   (cond ((featurep! :completion helm) #'helm-company)
                            ((featurep! :completion ivy)  #'counsel-company))
            "C-SPC"   #'company-complete-common-or-cycle
            [tab]     #'company-complete-common-or-cycle
            [backtab] #'company-select-previous)
          (:map company-search-map  ; applies to `company-filter-map' too
            "C-n"     #'company-select-next-or-abort
            "C-p"     #'company-select-previous-or-abort
            "C-j"     #'company-select-next-or-abort
            "C-k"     #'company-select-previous-or-abort
            "C-s"     (λ! (company-search-abort) (company-filter-candidates))
            "ESC"     #'company-search-abort)
          ;; TAB auto-completion in term buffers
          (:map comint-mode-map
            "C-SPC" #'company-complete)))

      (:when (featurep! :completion ivy)
        (:map (help-mode-map helpful-mode-map)
          :n "Q" #'ivy-resume)
        (:after ivy
          :map ivy-minibuffer-map
          "C-SPC" #'ivy-call-and-recenter  ; preview file
          "C-l"   #'ivy-alt-done
          "C-v"   #'yank)
        (:after counsel
          :map counsel-ag-map
          "C-SPC"    #'ivy-call-and-recenter ; preview
          "C-l"      #'ivy-done
          "C-c C-e"  #'+ivy/wgrep-occur      ; search/replace on results
          [backtab]  #'+ivy/wgrep-occur      ; search/replace on results
          [C-return] #'+ivy/git-grep-other-window-action)
        (:after swiper
          :map swiper-map
          [backtab] #'+ivy/wgrep-occur
          [C-return] #'+ivy/git-grep-other-window-action)))

(map! (:when (featurep! :editor fold)
        :nv "C-SPC" #'+fold/toggle)
      (:when (featurep! :editor format)
        :n "gQ" #'+format:region))

(map! :leader
      "w"    #'save-buffer
      "q"    #'kill-this-buffer
      "o"    #'find-file

      "b"    #'persp-switch-to-buffer
      "B"    #'switch-to-buffer
      "+"    #'evil-numbers/inc-at-pt
      "-"    #'evil-numbers/dec-at-pt
      ","    #'eshell

      "s"    #'doom/open-scratch-buffer
      "!"    #'doom/sudo-this-file
      ":"    #'eval-expression
      ";"    #'execute-extended-command
      "."    #'doom/open-private-config
      "m"    #'+popup/toggle
      "/"    #'evil-ex-nohighlight
      "\\"   #'toggle-truncate-lines

      :desc "show functions" "l" #'imenu

      :desc "Surrond With" "["    (lambda! (sp-rewrap-sexp (dc--get-sp-pair "[")))
      :desc "Surrond With" "{"    (lambda! (sp-rewrap-sexp (dc--get-sp-pair "{")))
      :desc "Surrond With" "("    (lambda! (sp-rewrap-sexp (dc--get-sp-pair "(")))
      :desc "Surrond With" "\""   (lambda! (sp-rewrap-sexp (dc--get-sp-pair "\"")))

      (:prefix ("r" . "Replace line")
        :desc "custom" "c" (lambda!
                            (evil-ex
                             (concat (dc--get-evil-ex-prefix) "s/\\<")))

        :desc "word" "w" (lambda!
                          (evil-ex
                           (concat
                            (dc--get-evil-ex-prefix)
                            "s/\\<" (thing-at-point 'word) "\\>/")))

        :desc "WORD" "W" (lambda!
                          (evil-ex
                           (concat
                            (dc--get-evil-ex-prefix)
                            "s/\\<" (thing-at-point 'symbol) "\\>/"))))

      (:prefix ("R" . "Replace buffer")
        :desc "custom" "c" (lambda! (evil-ex "%s/\\<"))
        :desc "word" "w" (lambda!
                          (evil-ex
                           (concat "%s/\\<" (thing-at-point 'word) "\\>/")))

        :desc "WORD" "W" (lambda!
                          (evil-ex
                           (concat "%s/\\<" (thing-at-point 'symbol) "\\>/"))))

      (:prefix ("p" . "Projects")
        :desc "open emacs.d" "e" #'+default/browse-emacsd
        :desc "open notes" "n" (lambda! (doom-project-browse "~/notes/"))
        :desc "open config" "c" (lambda! (doom-project-browse "~/.config/"))
        "R" #'projectile-replace
        "g" #'projectile-grep
        "o" #'projectile-switch-project
        "f" #'projectile-find-file
        "r" #'counsel-buffer-or-recentf)

      (:prefix ("c" . "Comments")
        "l"    #'evil-commentary-line)

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
          :desc "status" "s" #'magit-status
          :desc "add" "a" #'magit-stage-file
          :desc "add all" "A" #'magit-stage-modified
          :desc "unstage" "u" #'magit-unstage-file
          :desc "log" "l" #'magit-log-current
          :desc "commit" "c" #'magit-commit-create
          :desc "diff" "d" #'magit-diff-buffer-file
          :desc "diff worktree" "D" #'magit-diff-working-tree
          :desc "checkout" "b" #'magit-branch-or-checkout
          (:prefix ("S" . "stash")
            :desc "pop" "P" #'magit-stash-pop
            :desc "push" "p" #'magit-stash-worktree
            :desc "list" "l" #'magit-stash-list
            :desc "show" "s" #'magit-stash-show
            :desc "drop" "d" #'magit-stash-drop
            :desc "branch" "b" #'magit-stash-branch)))

      (:prefix ("e" . "Email")
        :desc "Go to mail" "g" #'=mu4e
        :desc "Compose mail" "c" #'compose-mail-other-window)

      (:prefix ("C" . "Code checking")
        :desc "toggle spell"   "s" #'flyspell-mode
        :desc "toggle lint"    "c" #'flycheck-mode
        :desc "list errors"    "l" #'flycheck-list-errors)

      (:prefix ("t" . "Org-mode")
        :desc "Agenda"         "a"  #'org-agenda
        :desc "Todo list"      "t"  #'org-todo-list
        :desc "Tags search"    "m"  #'org-tags-view
        :desc "Org capture"    "x"  #'org-capture
        :desc "Go to org file" "g"  (lambda!
                                     (counsel-find-file "~/org/"))
        :desc "Org store link" "l"  #'org-store-link
        :desc "View search"    "v"  #'org-search-view
        (:prefix ("c" . "Clock")
          :desc "Clock in" "c" #'counsel-org-clock-context
          :desc "Clock out" "C" #'org-clock-out
          :desc "Goto last" "g" #'counsel-org-clock-goto))

      (:prefix ("n" . "Noter")
        :desc "Run Noter" "n"    #'org-noter
        :desc "Open Index" "i"  (lambda! (find-file refs-bib))
        :desc "Open Reference" "r" (lambda! (counsel-find-file refs-pdfs))
        :desc "Open Notes" "o" (lambda! (find-file refs-notes))
        :desc "Insert Cite" "c" #'org-ref-insert-cite-with-completion)

      (:prefix ("d" . "Dictionary")
        :desc "Define at point" "d" #'define-word-at-point
        :desc "Define any word" "D" #'define-word
        :desc "Thesaurus" "t" #'powerthesaurus-lookup-word-dwim))

;; Minibuffer
(define-key! evil-ex-completion-map
  "C-a" #'move-beginning-of-line
  "C-b" #'backward-word
  "C-SPC" (if (featurep! :completion ivy)
              #'counsel-minibuffer-history
            #'helm-minibuffer-history))

(define-key! :keymaps +default-minibuffer-maps
  "C-v"    #'yank
  "C-z"    (λ! (ignore-errors (call-interactively #'undo)))
  "C-a"    #'move-beginning-of-line
  "C-b"    #'backward-word
  "C-r"    #'evil-paste-from-register
  ;; Scrolling lines
  "C-n"    #'next-line
  "C-p"    #'previous-line)
