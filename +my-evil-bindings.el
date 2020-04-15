;;; +my_evil_bindings.el --- Port of my init.vim -*- lexical-binding: t; -*-
;;;

(setq evil-collection-key-blacklist
      ;;; Code:
      (list "C-j" "C-k" "gd" "gf" "J" "K" "[" "]" "gz" "Spc" "gr" "gR"
            doom-leader-key doom-localleader-key
            doom-leader-alt-key doom-localleader-alt-key))

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

(dc--select-function-with-universal-arg arg-swiper-isearch
                                        #'swiper #'swiper-isearch)
(dc--select-function-with-universal-arg arg-backward-swiper-isearch
                                        #'swiper-backward #'swiper-isearch-backward)
(dc--select-function-with-universal-arg arg-at-point-swiper-isearch
                                        #'swiper-thing-at-point
                                        #'swiper-isearch-thing-at-point)
(dc--select-function-with-universal-arg arg-ebib-open-bibtex-file
                                        (lambda! (ebib refs-bib))
                                        (lambda! (let* ((file-list
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
   :desc "Top layer key map" "K" #'which-key-show-top-level
   :desc "External info manuals" "C-i" (lambda! (dc-find-external-manual "~/info")))

 :m   "]a"      #'evil-forward-arg
 :m   "[a"      #'evil-backward-arg
 :m   "]o"      #'outline-next-visible-heading
 :m   "[o"      #'outline-previous-visible-heading
 :n   "}"       #'next-buffer
 :n   "{"       #'previous-buffer
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
 :nv  "gc"      #'dc-comment-line-operator
 :nv  "gr"      #'dc-apply-ex-substitute-word
 :nv  "gR"      #'dc-apply-ex-substitute-WORD
 :nv  "gk"      #'dc-apply-ex-substitute-custom
 :nv  "C-a"     #'evil-numbers/inc-at-pt
 :nv  "C-S-a"   #'evil-numbers/dec-at-pt
 :v   "gp"      #'+evil/paste-preserve-register
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
 :nv  "/"       #'arg-swiper-isearch
 :nv  "?"       #'arg-backward-swiper-isearch
 :nv  "gn"      #'arg-at-point-swiper-isearch
 :nvi "M-/"     #'link-hint-open-link
 :nvi "C-u"     #'universal-argument
 :nv  "U"       #'undo-tree-visualize
 :nvi "C-c C-r" #'ivy-resume
 :nvm   "C-n"     #'evil-next-line
 :nvm   "C-p"     #'evil-previous-line

 ;; Use C-s (snipe) instead of C-t becaulse C-t is my stumpwm key
 :nvm "C-s" (lambda! ; make exclusive (more like till than from)
             (call-interactively #'avy-goto-char-in-line)
             (point))

 :nvm "C-f" #'avy-goto-char-in-line
 :nvm "C-/" #'avy-goto-char-2
 :m   "C-e" #'+evil/easymotion  ; lazy-load `evil-easymotion'

 :o "j"   #'evilem-motion-next-line
 :o "k"   #'evilem-motion-previous-line
 :o "C-w" #'evilem-motion-forward-word-begin
 :o "C-e" #'evilem-motion-forward-word-end
 :o "C-b" #'evilem-motion-backward-word-begin

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
          :i "C-p"    #'+company/dabbrev-code-previous
          :i "C-u"    #'company-math-symbols-unicode)
        (:after company
          (:map company-active-map
            "C-w"     nil  ; don't interfere with `evil-delete-backward-word'
            "C-n"     #'company-select-next
            "C-p"     #'company-select-previous
            "C-h"     #'company-show-doc-buffer
            "C-u"     #'company-previous-page
            "C-d"     #'company-next-page
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
          "C-SPC" #'ivy-partial-or-done ; preview file
          "C-j"   #'ivy-alt-done
          "C-k"   nil
          "C-r"   #'counsel-minibuffer-history
          "C-v"   #'yank
          "C-s"   #'ivy-mark
          "C--"   #'ivy-unmark
          "C-f"   #'ivy-toggle-marks
          "C-a"   #'ivy-toggle-calling)
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
          [C-return] #'+ivy/git-grep-other-window-action)))

(map! (:when (featurep! :editor fold)
        :nv "C-SPC" #'+fold/toggle)
      (:when (featurep! :editor format)
        :n "gQ" #'+format:region)
      (:when (featurep! :editor snippets)
        :i  "C-a" #'aya-expand
        :nv "C-a" #'aya-create
        :i  "C-s" #'yas-expand))

(dc--select-function-with-universal-arg arg-define-word
                                        #'define-word-at-point
                                        (lambda (word)
                                          (interactive "MWord: ")
                                          (define-word word 'wordnik)))

(dc--select-function-with-universal-arg arg-thesaurus-word
                                        #'powerthesaurus-lookup-word-at-point
                                        #'powerthesaurus-lookup-word)

(dc--select-function-with-universal-arg arg-wordnut-word
                                        #'wordnut-lookup-current-word
                                        #'wordnut-search)

(after! vterm
  (add-hook! 'vterm-mode-hook
             #'evil-emacs-state)

  (map! :map 'vterm-mode-map
        :n "a" #'evil-emacs-state
        :n "i" #'evil-emacs-state
        :nie "C-d" #'vterm-send-C-d
        :e "C-g" #'evil-normal-state))

(map! :leader
      "w"    #'save-buffer
      "q"    #'kill-this-buffer
      "o"    #'find-file
      "O"    #'counsel-locate

      "b"    #'persp-switch-to-buffer
      "B"    #'switch-to-buffer
      "+"    #'evil-numbers/inc-at-pt
      "-"    #'evil-numbers/dec-at-pt
      ","    (lambda (arg) (interactive "p")
               (cond ((= arg 4)
                      (call-interactively #'+vterm/toggle))
                     ((= arg 16)
                      (call-interactively #'+vterm/here))
                     (t (vterm))))
      "<"    #'counsel-switch-to-shell-buffer

      :desc "journal" "j"    (lambda! (dc-run-deft-in-workspace
                                 "Journal" "~/journal/"))

      "s"    #'doom/open-scratch-buffer
      :desc "Find file on server" "S" #'dc-find-file-on-server
      "!"    #'doom/sudo-this-file
      ":"    #'helm-eval-expression
      ";"    #'execute-extended-command
      :desc ".doom.d" "."    (lambda! (dc-open-in-workspace "Doom" "~/.doom.d"))
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
      :desc "Open Notes"   "N"    (lambda! (dc-open-in-workspace "Notes" notes-dir))

      (:prefix ("r" . "Replace line")
        :desc "custom" "c" (lambda!
                            (evil-ex
                             (concat (dc--get-evil-ex-prefix) "s/")))

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
        :desc "custom" "c" (lambda! (evil-ex "%s/"))
        :desc "word" "w" (lambda!
                          (evil-ex
                           (concat "%s/\\<" (thing-at-point 'word) "\\>/")))

        :desc "WORD" "W" (lambda!
                          (evil-ex
                           (concat "%s/\\<" (thing-at-point 'symbol) "\\>/"))))

      (:prefix ("p" . "Projects")
        :desc "open emacs.d" "e" (lambda! (dc-open-in-workspace "Emacs" "~/.emacs.d"))
        :desc "open config" "c" (lambda! (dc-open-in-workspace "Config" "~/.config/"))
        "R" #'projectile-replace
        "g" #'+ivy/project-search
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
        :desc "Compose mail" "c" #'mu4e-compose-new)

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
        :desc "Go to org file" "g"  (lambda! (dc-open-org-file-in-workspace
                                        "Org" org-directory))
        :desc "Org store link" "l"  #'org-store-link
        :desc "View search"    "v"  #'org-search-view
        :desc "Deft"           "d"  (lambda! (dc-run-deft-in-workspace
                                        "Org" org-directory))
        (:prefix ("c" . "Clock")
          :desc "Pomodoro timer" "p" #'org-pomodoro
          :desc "Clock in"       "c" #'counsel-org-clock-context
          :desc "Clock out"      "C" #'org-clock-out
          :desc "Clock in last"  "l" #'org-clock-in-last
          :desc "Cancel"         "x" #'org-clock-cancel
          :desc "Goto last"      "g" #'counsel-org-clock-goto))

      (:prefix ("n" . "Reference Notes")
        :desc "Open Index" "i"  (lambda! (dc-open-in-workspace
                                    "References" refs-bib))
        :desc "Ebib" "e" #'arg-ebib-open-bibtex-file
        :desc "Open Reference" "r" (lambda! (dc-open-in-workspace
                                       "References" refs-pdfs))
        :desc "Go to Note" "g" (lambda! (dc-open-org-file-in-workspace
                                             "References" refs-notes))
        :desc "Open reading list" "l" (lambda! (dc-open-in-workspace
                                          "References"
                                          (concat refs-notes "readinglist.org")))
        :desc "Deft" "d" (lambda! (dc-run-deft-in-workspace
                             "References" refs-notes)))

      (:prefix ("d" . "Define")
        :desc "Define at point"  "d" #'arg-define-word
        :desc "Define any word"  "D" #'mw-thesaurus-lookup-at-point
        :desc "Thesaurus"        "t" #'arg-thesaurus-word
        :desc "Wordnut at point" "w" #'arg-wordnut-word
        :desc "Biblio lookup"    "b" #'biblio-lookup
        :desc "Search duck"      "s" #'counsel-search))
