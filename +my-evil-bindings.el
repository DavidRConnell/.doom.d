;;; +my_evil_bindings.el --- Port of my init.vim -*- lexical-binding: t; -*-
;;;

(setq evil-collection-key-blacklist
      ;;; Code:
      (list "C-j" "C-k" "gd" "gf" "J" "K" "[" "]" "gz" "Spc"
            doom-leader-key doom-localleader-key
            doom-leader-alt-key doom-localleader-alt-key))

(when (featurep! :editor evil +everywhere)
  ;; Have C-u behave similarly to `doom/backward-to-bol-or-indent'.
  ;; NOTE SPC u replaces C-u as the universal argument.
  (map! :i "C-u" #'doom/backward-kill-to-bol-and-indent
        :i "C-w" #'backward-kill-word
        ;; Vimmish ex motion keys
        :i "C-b" #'backward-word
        :i "C-f" #'forward-word)

  ;; Minibuffer
  (define-key! evil-ex-completion-map
    "C-a" #'move-beginning-of-line
    "C-b" #'backward-word
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
    "C-f"    #'forward-word
    "C-r"    #'evil-paste-from-register
    "C-u"    #'doom/backward-kill-to-bol-and-indent
    "C-v"    #'yank
    "C-w"    #'backward-kill-word
    "C-z"    (λ! (ignore-errors (call-interactively #'undo)))
    ;; Scrolling lines
    "C-n"    #'next-line
    "C-p"    #'previous-line
    "C-j"  #'scroll-up-command
    "C-k"  #'scroll-down-command)

  (define-key! read-expression-map
    "C-n" #'next-line-or-history-element
    "C-p" #'previous-line-or-history-element))

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
   :n "o"       #'ace-link-help)
 (:after helpful :map helpful-mode-map
   :n "o"       #'ace-link-help)
 (:after info :map Info-mode-map
   :n "o"       #'ace-link-info)
 (:after apropos :map apropos-mode-map
   :n "o"       #'ace-link-help
   :n "TAB"     #'forward-button
   :n [tab]     #'forward-button
   :n [backtab] #'backward-button)
 (:after view :map view-mode-map
   [escape]  #'View-quit-all)
 (:after man :map Man-mode-map
   :n "q"    #'kill-current-buffer)

 (:prefix "C-h"
   :desc "Find info manual" "RET" #'info-display-manual
   :desc "Top layer key map" "K" #'which-key-show-top-level
   :desc "External info manuals" "C-i" (lambda! (dc-find-external-manual "~/info")))

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
 :nv "Q"     #'+evil:apply-macro
 :nv "C-q"   #'evil-execute-macro
 :nv "gx"    #'evil-exchange
 :nv "gK"    #'+lookup/documentation
 :nv "gc"    #'dc-comment-line-operator
 :nv "gr"    #'dc-apply-ex-substitute-word
 :nv "gR"    #'dc-apply-ex-substitute-WORD
 :nv "gk"    #'dc-apply-ex-substitute-custom
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
 :n "C-j"    #'evil-scroll-page-down
 :n "C-k"    #'evil-scroll-page-up
 :v "C-j"    #'avy-goto-line-below
 :v "C-k"    #'avy-goto-line-above
 :nv "C-m"   #'evil-goto-mark
 :nv "ZZ"    #'save-buffers-kill-terminal
 :nv "/" #'swiper
 :nv "?" #'swiper-backward
 :nv "gn" #'swiper-thing-at-point
 :nvi "M-/" #'ace-link
 :nvi "C-u" #'universal-argument

 ;; Use C-s (snipe) instead of C-t becaulse C-t is my stumpwm key
 :nvm "C-s" (lambda! ; make exclusive (more like till than from)
             (call-interactively #'avy-goto-char-in-line)
             (point))

 :nvm "C-f" #'avy-goto-char-in-line

 :nv "C-/" #'avy-goto-char-timer
 (:after evil-easymotion
   :map evilem-map
   "/" (evilem-create #'evil-ex-search-next
                      :pre-hook (save-excursion (call-interactively #'swiper))
                      :bind ((evil-search-wrap)))
   "?" (evilem-create #'evil-ex-search-previous
                      :pre-hook (save-excursion (call-interactively #'swiper-backward))
                      :bind ((evil-search-wrap))))

 (:when (featurep! :ui workspaces)
   :nv "M-t"   #'+workspace/new
   :nv "M-T"   #'+workspace/display
   :nv "M-q"   #'+workspace/delete
   :nv "M-1"   (λ! (+workspace/switch-to 0))
   :nv "M-2"   (λ! (+workspace/switch-to 1))
   :nv "M-3"   (λ! (+workspace/switch-to 2))
   :nv "M-4"   (λ! (+workspace/switch-to 3))
   :nv "M-5"   (λ! (+workspace/switch-to 4))
   :nv "M-6"   (λ! (+workspace/switch-to 5))
   :nv "M-7"   (λ! (+workspace/switch-to 6))
   :nv "M-8"   (λ! (+workspace/switch-to 7))
   :nv "M-9"   (λ! (+workspace/switch-to 8))
   :nv "M-0"   #'+workspace/switch-to-final))

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
          "C-SPC" #'ivy-partial-or-done ; preview file
          "C-l"   #'ivy-alt-done
          "C-v"   #'yank)
        (:after counsel
          :map counsel-ag-map
          "C-SPC"    #'ivy-partial-or-done ; preview
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
        :n "gQ" #'+format:region)
      (:when (featurep! :editor snippets)
        :i "C-e" #'aya-expand
        :nv "C-e" #'aya-create
        :i "C-s" #'yas-expand))

(map! :leader
      "w"    #'save-buffer
      "q"    #'kill-this-buffer
      "o"    #'find-file

      "b"    #'persp-switch-to-buffer
      "B"    #'switch-to-buffer
      "+"    #'evil-numbers/inc-at-pt
      "-"    #'evil-numbers/dec-at-pt
      ","    #'eshell
      "e"    #'+eval/line-or-region

      "s"    #'doom/open-scratch-buffer
      :desc "Find file on server" "S" #'dc-find-file-on-server
      "!"    #'doom/sudo-this-file
      ":"    #'eval-expression
      ";"    #'execute-extended-command
      "."    (lambda! (projectile-switch-project-by-name "~/.doom.d"))
      "m"    #'+popup/toggle
      "/"    #'evil-ex-nohighlight
      "\\"   #'toggle-truncate-lines

      :desc "Find file in project"  "SPC"  #'projectile-find-file
      :desc "Search for symbol in project" "*" #'+default/search-project-for-symbol-at-point
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
        :desc "open emacs.d" "e" (lambda! (projectile-switch-project-by-name
                                           "~/.emacs.d"))
        :desc "open config" "c" (lambda! (doom-project-browse "~/.config/"))
        "R" #'projectile-replace
        "g" #'projectile-grep
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

      (:prefix ("M" . "Mail")
        :desc "Go to mail" "g" #'=mu4e
        :desc "Compose mail" "c" #'mu4e-compose-new)

      (:prefix ("C" . "Code checking")
        :desc "toggle spell"     "s" #'flyspell-mode
        :desc "toggle lint"      "c" #'flycheck-mode
        :desc "toggle langcheck" "g" #'langtool-check
        :desc "list errors"      "l" #'flycheck-list-errors)

      (:prefix ("t" . "Org-mode")
        :desc "Agenda"         "a"  #'org-agenda
        :desc "Todo list"      "t"  #'org-todo-list
        :desc "Tags search"    "m"  #'org-tags-view
        :desc "Org capture"    "x"  #'org-capture
        :desc "Go to org file" "g"  #'dc-open-org-file
        :desc "Org store link" "l"  #'org-store-link
        :desc "View search"    "v"  #'org-search-view
        (:prefix ("c" . "Clock")
          :desc "Clock in" "c" #'counsel-org-clock-context
          :desc "Clock out" "C" #'org-clock-out
          :desc "Goto last" "g" #'counsel-org-clock-goto))

      (:prefix ("n" . "Noter")
        :desc "Run Noter" "n"    #'org-noter
        :desc "Open Index" "i"  (lambda! (dc-open-in-workspace "Notes" refs-bib))
        :desc "Open Reference" "r" (lambda! (dc-open-in-workspace "Notes" refs-pdfs))
        :desc "Open Notes" "o" (lambda! (dc-open-in-workspace "Notes" refs-notes))
        :desc "Insert Cite" "c" #'org-ref-insert-cite-with-completion
        :desc "Deft" "d" #'deft)

      (:prefix ("d" . "Dictionary")
        :desc "Define at point"     "d" #'define-word-at-point
        :desc "Define any word"     "D" #'define-word
        :desc "Thesaurus"           "t" #'powerthesaurus-lookup-word-dwim
        :desc "Search with wordnut" "w" #'wordnut-lookup-current-word))
