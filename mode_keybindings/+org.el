;;; +org.el --- description -*- lexical-binding: t; -*-

(map!
 (:after (org evil-org)
  (:map (org-mode-map evil-org-mode-map)
   :nv "C-j" #'org-forward-heading-same-level
   :nv "C-k" #'org-backward-heading-same-level
   :nv "H" #'org-beginning-of-line
   :nv "j" #'evil-next-visual-line
   :nv "k" #'evil-previous-visual-line
   :nv "zn" #'org-toggle-narrow-to-subtree
   :nv "C-i" #'better-jumper-jump-forward
   :n "C-;" #'iedit-mode
   ;; org-ref open cite bindings
   :nv "s-p" #'org-ref-open-pdf-at-point
   :nv "s-n" #'org-ref-open-notes-at-point
   :nv "s-u" #'org-ref-open-url-at-point
   :nv "s-c" #'org-ref-open-citation-at-point
   :nv "s-s" #'org-ref-google-scholar-at-point
   :nv "s-d" #'dc-org-ref-get-key-at-point
   :n  "za"  #'spell-fu-word-add
   :n  "zr"  #'spell-fu-word-remove
   (:prefix "C-e"
    "h" #'avy-org-goto-heading-timer)
   (:localleader
    "n" #'org-add-note
    "p" #'org-priority
    "A" #'org-archive-subtree
    "\\" #'writeroom-mode
    "e" nil
    (:prefix ("e" . "export")
     "l" #'org-ref-ivy-insert-label-link
     "r" #'org-ref-ivy-insert-ref-link
     "c" (dc-arg-cmd #'org-ref-ivy-insert-cite-link
                     #'org-ref-insert-cite-with-completion)
     "b" #'org-ref-insert-bibliography-link
     "s" #'org-ref-insert-bibliographystyle-link
     "i" #'org-ref-index
     "e" #'org-ref
     "o" #'dc-plop-ref-outline
     "p" (cmd! (org-open-file
             (funcall org-ref-get-pdf-filename-function
                      (file-name-sans-extension (buffer-name)))))
     "n" (cmd! (org-ref-open-notes-at-point
             (format "%s" (symbol-at-point)))))
    (:prefix "c"
     "p" #'org-pomodoro)
    "d" nil
    (:prefix ("d" . "drill")
     "d" #'org-drill
     "t" #'org-drill-tree
     "c" #'org-drill-cram
     "r" #'org-drill-resume)
    (:prefix ("G" . "grammar check")
     "r" (dc-arg-cmd (cmd! (evil-visual-char)
                        (evil-org-inner-subtree)
                        (langtool-check-buffer))
                     #'langtool-check-buffer)
     "c" #'langtool-correct-buffer
     "d" #'langtool-check-done)
    (:prefix "l"
     "h" #'counsel-org-link)
    (:prefix "m"
     "x" #'org-roam-capture))))

 (:after org-agenda
  (:map org-agenda-mode-map
   :m "J" #'evil-scroll-line-down
   :m "K" #'evil-scroll-line-up
   :m "C-j" #'org-agenda-forward-block
   :m "C-k" #'org-agenda-backward-block
   :m "/" #'org-agenda-filter-by-tag
   :m "L" #'evil-end-of-line
   :m "H" #'evil-beginning-of-line
   :m "RET" #'+org/dwim-at-point
   (:prefix "C-c"
    "C-c" #'org-agenda-set-tags
    "." #'org-agenda-date-prompt)
   (:localleader
    "n" #'org-agenda-add-note
    "p" #'org-agenda-priority
    "A" #'org-agenda-archive
    (:prefix ("c" . "Clock")
     "c" #'org-agenda-clock-in
     "C" #'org-agenda-clock-out
     "g" #'org-clock-goto
     "p" #'org-pomodoro
     "r" #'org-agenda-clockreport-mode)))))
