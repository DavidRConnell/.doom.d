;;; init.el -*- lexical-binding: t; -*-

;; Copy this file to ~/.doom.d/init.el or ~/.config/doom/init.el ('doom
;; quickstart' will do this for you). The `doom!' block below controls what
;; modules are enabled and in what order they will be loaded. Remember to run
;; 'doom refresh' after modifying it.
;;
;; More information about these modules (and what flags they support) can be
;; found in modules/README.org.

(doom! :completion
       company           ; the ultimate code completion backend
       (ivy              ; a search engine for love and life
        +prescient
        +childframe)

       :ui
       deft              ; notational velocity for Emacs
       doom              ; what makes DOOM look the way it does
       doom-dashboard    ; a nifty splash screen for Emacs
       fill-column       ; a `fill-column' indicator
       hl-todo           ; highlight TODO/FIXME/NOTE tags
       indent-guides     ; highlighted indent columns
       modeline          ; snazzy, Atom-inspired modeline, plus API
       nav-flash         ; blink the current line after jumping
       ophints           ; highlight the region an operation acts on
       (popup            ; tame sudden yet inevitable temporary windows
        +all             ; catch all popups that start with an asterix
        +defaults)       ; default popup rules
       ligatures         ; replace bits of code with pretty symbols
       vc-gutter         ; vcs diff in the fringe
       vi-tilde-fringe   ; fringe tildes to mark beyond EOB
       window-select     ; visually switch windows
       workspaces        ; tab emulation, persistence & separate workspaces
       zen               ; distraction-free coding or writing

       :editor
       (evil +everywhere); come to the dark side, we have cookies
       file-templates    ; auto-snippets for empty files
       fold              ; (nigh) universal code folding
       (format +onsave)  ; automated prettiness
       snippets          ; my elves. They type so I don't have to
       word-wrap         ; soft wrapping with language-aware indent

       :emacs
       (dired            ; making dired pretty [functional]
        +icons           ; colorful icons for dired-mode
        +ranger)
       electric          ; smarter, keyword-based electric-indent
       (undo +tree)      ; persistent, smarter undo for your inevitable mistakes
       vc                ; version-control and Emacs, sitting in a tree

       :term
       shell
       vterm

       :checkers
       (syntax           ; tasing you for every semicolon you forget
        +childframe)
       spell             ; tasing you for misspelling mispelling
       grammar           ; tasing grammar mistake every you make

       :tools
       (debugger +lsp)   ; FIXME stepping through code, to help you add bugs
       editorconfig      ; let someone else argue about tabs vs spaces
       (eval +overlay)   ; run code, run (also, repls)
       (lookup           ; helps you navigate your code and documentation
        +docsets)        ; ...or in Dash docsets locally
       (lsp +peek)
       magit             ; a git porcelain for Emacs
       make              ; run make tasks from Emacs
       pdf               ; pdf enhancements

       :lang
       (cc +lsp)     ; C/C++/Obj-C madness
       common-lisp   ; if you've seen one lisp, you've seen them all
       data          ; config/data formats
       emacs-lisp    ; drown in parentheses
       (ess +lsp)    ; emacs speaks statistics
       (julia +lsp)  ; a better, faster MATLAB
       (latex        ; writing papers in Emacs has never been so fun
        +lsp
        +latexmk
        +fold)
       ;; ledger     ; an accounting system in Emacs
       ;; scheme
       markdown      ; writing docs for people to ignore
       web
       (org          ; organize your plain life in plain text
        +pomodoro    ; be fruitful with the tomato technique
        +present     ; Emacs for presentations
        +gnuplot
        +pandoc
        +hugo
        +roam)
       (python +lsp) ; beautiful is better than ugly
       (sh +lsp)     ; she sells (ba|z|fi)sh shells on the C xor

       :email
       (mu4e +gmail)

       :app
       (rss +org)

       :config
       (default +smartparens))
