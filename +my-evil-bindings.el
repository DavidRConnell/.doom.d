;;; +my_evil_bindings.el --- Port of my init.vim -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;;             Things TODO main keybindings
;;;                         TAB
;;;                            Tab not tabbing in insert mode
;;;                            checkout company more
;;;                         Replace functions
;;;                         Git wrappers
;;;                         Toggles: highlight
;;;                         Relative numbers

(setq evil-collection-key-blacklist
      ;;; Code:
      (list "C-j" "C-k" "gd" "gf" "K" "[" "]" "gz"
            doom-leader-key doom-localleader-key
            doom-leader-alt-key doom-localleader-alt-key))

(map!
        ;; Global evil keybinds
        :m  "]a"    #'evil-forward-arg
        :m  "[a"    #'evil-backward-arg
        :m  "]o"    #'outline-next-visible-heading
        :m  "[o"    #'outline-previous-visible-heading
        :n  "]b"    #'next-buffer
        :n  "}"     #'next-buffer
        :n  "[b"    #'previous-buffer
        :n  "{"     #'previous-buffer
        :n  "zx"    #'kill-this-buffer
        :n  "ZX"    #'bury-buffer
        :n  "gp"    #'+evil/reselect-paste
        :n  "g="    #'widen
        :v  "g="    #'+evil:narrow-buffer
        :nv "z="    #'flyspell-correct-word-generic
        :nv "g@"    #'+evil:apply-macro
        :nv "gx"    #'evil-exchange
        :nv "C-a"   #'evil-numbers/inc-at-pt
        :nv "C-S-a" #'evil-numbers/dec-at-pt
        :v  "gp"    #'+evil/paste-preserve-register
        :v  "@"     #'+evil:apply-macro
        :v  "."     #'+evil:apply-macro
        :v  "<"     #'+evil/visual-dedent  ; vnoremap < <gv
        :v  ">"     #'+evil/visual-indent  ; vnoremap > >gv
        :nv ";"     #'evil-ex
        :nv "H"     #'evil-first-non-blank
        :nv "L"     #'evil-end-of-line
        :nv "gd"    #'find-function-at-point
        :nv "K"     #'evil-scroll-line-up
        :nv "J"     #'evil-scroll-line-down
        :nv "s"     #'evil-substitute

        ;; Smarter newlines
        :i [remap newline] #'newline-and-indent  ; auto-indent on newline
        :i "C-j"           #'+default/newline    ; default behavior

      :m  "gs"    #'+evil/easymotion  ; lazy-load `evil-easymotion'
      (:after evil-easymotion
        :map evilem-map
        "a" (evilem-create #'evil-forward-arg)
        "A" (evilem-create #'evil-backward-arg)
        "s" (evilem-create #'evil-snipe-repeat
                           :name 'evil-easymotion-snipe-forward
                           :pre-hook (save-excursion (call-interactively #'evil-snipe-s))
                           :bind ((evil-snipe-scope 'buffer)
                                  (evil-snipe-enable-highlight)
                                  (evil-snipe-enable-incremental-highlight)))
        "S" (evilem-create #'evil-snipe-repeat
                           :name 'evil-easymotion-snipe-backward
                           :pre-hook (save-excursion (call-interactively #'evil-snipe-S))
                           :bind ((evil-snipe-scope 'buffer)
                                  (evil-snipe-enable-highlight)
                                  (evil-snipe-enable-incremental-highlight)))
        "SPC" #'avy-goto-char-timer
        "/" (evilem-create #'evil-ex-search-next
                           :pre-hook (save-excursion (call-interactively #'evil-ex-search-forward))
                           :bind ((evil-search-wrap)))
        "?" (evilem-create #'evil-ex-search-previous
                           :pre-hook (save-excursion (call-interactively #'evil-ex-search-backward))
                           :bind ((evil-search-wrap))))

      (:after evil-snipe
        :map evil-snipe-parent-transient-map
        "C-;" (λ! (require 'evil-easymotion)
                  (call-interactively
                   (evilem-create #'evil-snipe-repeat
                                  :bind ((evil-snipe-scope 'whole-buffer)
                                         (evil-snipe-enable-highlight)
                                         (evil-snipe-enable-incremental-highlight))))))
)

(map! (:when (featurep! :completion company)
        :i "C-@"      #'+company/complete
        :i "C-SPC"    #'+company/complete
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
            "C-SPC"   #'company-complete-common
            "TAB"     #'company-complete-common-or-cycle
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
            "TAB" #'company-complete
            [tab] #'company-complete)))

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
          [C-return] (+ivy-do-action! #'+ivy-git-grep-other-window-action))
        (:after swiper
          :map swiper-map
          [backtab] #'+ivy/wgrep-occur
          [C-return] (+ivy-do-action! #'+ivy-git-grep-other-window-action))
        (:after swiper
          :map swiper-map
          [backtab] #'+ivy/wgrep-occur)))

(map! :leader
        "w"    #'save-buffer
        "q"    #'kill-this-buffer
        "o"    #'find-file
        "h"    #'evil-window-left
        "j"    #'evil-window-down
        "k"    #'evil-window-up
        "l"    #'evil-window-right
        "n"    #'evil-window-next
        "p"    #'evil-window-prev
         ;
        "b"    #'switch-to-buffer
        "+"    #'evil-numbers/inc-at-pt
        "-"    #'evil-numbers/dec-at-pt
        "e"    #'eval-defun

        "s"    #'doom/open-scratch-buffer
        "!"    #'doom/sudo-this-file
        ":"    #'eval-expression
        ";"    #'execute-extended-command
        "-"    #'+popup/toggle
        "/"    #'evil-ex-nohighlight
        "S"    #'flyspell-mode
        "C"    #'flycheck-mode

        (:prefix ("c" . "Comments")
          "l"    #'evil-commentary-line)
        (:prefix ("g" . "git")
          :desc "Revert" "R" #'vc-revert
            (:when (featurep! :ui vc-gutter)
                :desc "Git revert hunk"           "r"   #'git-gutter:revert-hunk
                :desc "Git stage hunk"            "h"   #'git-gutter:stage-hunk
                :desc "Git time machine"          "t"   #'git-timemachine-toggle
                :desc "Jump to next hunk"         "]"   #'git-gutter:next-hunk
                :desc "Jump to previous hunk"     "["   #'git-gutter:previous-hunk)
            (:when (featurep! :tools magit)
              :desc "status" "s" #'magit-status
              :desc "add" "a" #'magit-stage-file
              :desc "add all" "A" #'magit-stage-modified
              :desc "unstage" "u" #'magit-unstage-file
              :desc "log" "l" #'magit-log
              :desc "commit" "c" #'magit-commit
              :desc "diff" "d" #'magit-diff-buffer-file
              :desc "diff worktree" "D" #'magit-diff-working-tree
              :desc "checkout" "b" #'magit-branch-or-checkout
              (:prefix ("S" . "stash")
                :desc "pop" "P" #'magit-stash-pop
                :desc "push" "p" #'magit-stash-worktree
                :desc "list" "l" #'magit-stash-list
                :desc "show" "s" #'magit-stash-show
                :desc "drop" "d" #'magit-stash-drop
                :desc "branch" "b" #'magit-stash-branch))))

;;; +my_evil_bindings.el ends here
