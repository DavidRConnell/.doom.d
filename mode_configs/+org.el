(setq org-directory "~/org/")

(setq org-enforce-todo-dependencies t)
(add-hook 'org-todo-hook
          'org-reset-subtasks)

(defun org-reset-subtasks ()
  (save-restriction
    (save-excursion
      (org-up-element)
      (org-narrow-to-subtree)
      (while (not (eobp))
        (org-next-visible-heading 1)
        (org-todo (org-get-todo-sequence-head
                   (org-get-todo-state)))))))

(setq org-log-into-drawer t)
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "MAYBE(m)" "LATER(l)"
                  "STAGNANT(s)" "WAITING(w@)" "DOING(d!/)"
                  "|" "DONE(D!/)")
        (sequence "NOTES(N)" "|")
        (sequence "FIX(f)" "FIXING(i!/)" "|" "FIXED(F!/)")
        (sequence "[ ]([)" "[-](-!/)" "|" "[x](x!/)")))

(setq org-todo-keyword-faces
      '(("TODO" . (:foreground "#98be65" :weight bold))
        ("NEXT" . (:foreground "#da8548" :weight bold))
        ("DOING" . (:foreground "#bf5150" :weight bold))
        ("FIX" . (:foreground "#98be65" :weight bold))
        ("FIXING" . (:foreground "#bf5150" :weight bold))
        ("MAYBE" . (:foreground "#ecbe7b" :weight bold))
        ("WAITING" . (:foreground "#46d9ff" :weight bold))
        ("LATER" . (:foreground "#46d9ff" :weight bold))
        ("STAGNANT" . (:foreground "#46d9ff" :weight bold))
        ("NOTES" . (:foreground "#d2691e" :weight bold))
        ("[ ]" . (:foreground "#98be65" :weight bold))
        ("[-]" . (:foreground "#ecbe7b" :weight bold))))

(setq org-tag-alist '((:startgroup . nil)
                      ("@rush" . ?r)
                      ("@home" . ?h)
                      ("@computer" . ?c)
                      ("@phone" . ?p)
                      ("@errand" . ?e)
                      ("@read" . ?d)
                      (:endgroup . nil)
                      ("Bob" . ?B)
                      ("Stats" . ?S)
                      ("DataMgt" . ?J)
                      ("Andrew" . ?A)
                      (:startgrouptag)
                      ("work" . ?w)
                      (:grouptags)
                      ("{w_.+}" . ?W)
                      (:endgrouptag)
                      ("personal" . ?o)
                      ("grad" . ?g)))

(add-hook 'org-after-todo-statistics-hook
          'org-auto-complete-todo)

(defun org-auto-complete-todo (n-done n-not-done)
  (org-back-to-heading t)
  (let ((state (org-get-todo-state)))
    (if state
        (cond ((= n-not-done 0)
               (cond ((or (string-match state "DOING")
                          (string-match state "TODO")
                          (string-match state "NEXT"))
                      (org-todo "DONE"))
                     ((or (string-match state "FIX")
                          (string-match state "FIXING"))
                      (org-todo "FIXED"))
                     ((or (string-match state "[ ]")
                          (string-match state "[-]"))
                      (org-todo "[x]"))))
              ((and (> n-done 0) (> n-not-done 0))
               (cond ((or (string-match state "TODO")
                          (string-match state "NEXT")
                          (string-match state "DONE"))
                      (org-todo "DOING"))
                     ((or (string-match state "FIX")
                          (string-match state "FIXED"))
                      (org-todo "FIXING"))
                     ((or (string-match state "[ ]")
                          (string-match state "[x]"))
                      (org-todo "[-]"))))
              ((= n-done 0)
               (cond ((or (string-match state "DOING")
                          (string-match state "DONE"))
                      (org-todo "TODO"))
                     ((or (string-match state "FIXING")
                          (string-match state "FIXED"))
                      (org-todo "FIX"))
                     ((or (string-match state "[-]")
                          (string-match state "[x]")))))))))

;; (defmacro make-org-capture-file-vars (files)
;;   (dolist (f files)
;;     `(defvar ,(concat "+org-capture-" f "-file")
;;        (concat org-directory ,f ".org"))))

;; (make-org-capture-file-vars ("books" "habits"))

(defvar +org-capture-books-file
  (concat org-directory "books.org"))

(defvar +org-capture-habits-file
  (concat org-directory "habits.org"))

(defvar +org-capture-inbox-file
  (concat org-directory "inbox.org"))

(defvar +org-capture-running-file
  (concat org-directory "running.org"))

(setq org-capture-templates
      '(("t" "Todo" entry
         (file +org-capture-inbox-file)
         "* TODO %?")
        ("l" "Link" entry
         (file +org-capture-inbox-file)
         "* TODO %?\n%a")
        ("n" "Notes" entry
         (file+headline +org-capture-notes-file "Inbox")
         "* %u\n%?")
        ("b" "Books" entry
         (file+headline +org-capture-books-file "Inbox")
         "* [ ] %?\n%^{AUTHOR}p")
        ("h" "Habits" entry
         (file+headline +org-capture-habits-file "Inbox")
         "* TODO %?\nSCHEDULED: <%<%Y-%m-%d %a> %^{repeat|+1d}>\n:PROPERTIES:\n:STYLE: habit\n:END:")
        ("r" "Run Week" entry
         (file +org-capture-running-file)
         "* Week %<%W>\n%^{Mon}%t\n%^{Tue}%t\n%^{Wed}%t"
         :prepend t :immediate-finish t)))

(setq org-refile-targets '((nil :maxlevel . 9)
                           (org-agenda-files :maxlevel . 9)))

(setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)

(setq org-agenda-compact-blocks t)
(setq org-agenda-custom-commands
      '((" " "Agenda"
        ((agenda "" nil)
         (todo "DOING|[-]|FIXING"
               ((org-agenda-overriding-header "Doing")))
         (todo "NEXT|FIX|WAITING"
               ((org-agenda-overriding-header "Next")))
         (tags "Bob|Andrew"
               ((org-agenda-overriding-header "Device Meeting")))
         (tags "Stats|DataMgt"
               ((org-agenda-overriding-header "Stats/data Meeting"))))
        nil)))
