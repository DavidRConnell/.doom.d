(defun turn-off-flycheck ()
  (flycheck-mode -1))

(setq org-directory "~/org/")
(setq org-agenda-files (list "~/org/"))

(after! org
  ;; (require 'org-ref)

  (setq org-startup-folded t)
  (setq org-startup-truncated nil)
  (setq org-pretty-entities t)

  (add-to-list 'org-file-apps
               '("\\.pdf\\'" . (lambda (_file link)
                                 (call-process "xdg-open" nil 0 nil link))))

  (setq counsel-org-clock-default-action 'clock-in)

  (add-hook! 'org-mode-hook
             #'+org-pretty-mode
             #'turn-off-auto-fill
             #'visual-line-mode
             #'turn-off-flycheck
             (lambda () (hl-fill-column-mode -1))
             #'flyspell-mode)

  (setq org-enforce-todo-dependencies t)
  (add-hook! 'org-todo-repeat-hook
             #'org-reset-subtasks)

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
        '((sequence "TODO(t)"
                    "NEXT(n)"
                    "EMAIL(e)"
                    "MAYBE(m)"
                    "LATER(l)"
                    "STAGNANT(s)"
                    "WAITING(w@)"
                    "CANCELLED(c)"
                    "|" "DONE(d!/)")
          (type "NOTES(N)")
          (sequence "[ ]([)" "[-](-!/)" "|" "[x](x!/)")))

  (setq org-todo-keyword-faces
        '(("TODO" . (:foreground "#98be65" :weight bold))
          ("NEXT" . (:foreground "#da8548" :weight bold))
          ("EMAIL" . (:foreground "#da8548" :weight bold))
          ("MAYBE" . (:foreground "#ecbe7b" :weight bold))
          ("WAITING" . (:foreground "#46d9ff" :weight bold))
          ("CANCELLED" . (:foreground "#46d9ff" :weight bold))
          ("LATER" . (:foreground "#46d9ff" :weight bold))
          ("STAGNANT" . (:foreground "#bf5150" :weight bold))
          ("NOTES" . (:foreground "#d2691e" :weight bold))
          ("[ ]" . (:foreground "#98be65" :weight bold))
          ("[-]" . (:foreground "#ecbe7b" :weight bold))))

  (setq org-tag-alist '((:startgroup . nil)
                        ("@office" . ?o)
                        ("@home" . ?h)
                        ("@computer" . ?c)
                        ("@errand" . ?e)
                        ("@read" . ?r)
                        (:endgroup . nil)
                        (:startgrouptag . nil)
                        ("device" . ?d)
                        (:grouptags)
                        ("Bob" . ?B)
                        ("Tim" . ?T)
                        (:endgrouptag . nil)
                        (:startgrouptag . nil)
                        ("big meeting" . ?b)
                        (:grouptags)
                        ("Stats" . ?S)
                        ("DataMgt" . ?D)
                        (:endgrouptag . nil)
                        (:startgrouptag . nil)
                        ("Andrew" . ?A)
                        ("Jeff" . ?G)
                        ("Chris" . ?C)
                        (:startgrouptag . nil)
                        ("work" . ?w)
                        (:grouptags)
                        ("{w_.+}" . ?W)
                        (:endgrouptag . nil)
                        (:startgrouptag . nil)
                        ("personal" . ?p)
                        (:grouptags)
                        ("{p_.+}" . ?P)
                        (:endgrouptag . nil)
                        ("grad" . ?g)))


  ;; (add-hook 'org-after-todo-statistics-hook
  ;;           'org-auto-complete-todo)

  (defun org-auto-complete-todo (n-done n-not-done)
    (org-back-to-heading t)
    (let ((state (org-get-todo-state)))
      (if (member state (list "TODO" "DONE" "STAGNANT" "NEXT"))
          (cond
           ((= n-not-done 0)
            (org-todo (org-get-todo-sequence-tail state)))
           ((org-stagnant-project-p)
            (org-todo "STAGNANT"))
           ((string= state "NEXT"))
           (t (org-todo (org-get-todo-sequence-head state)))))))

  (defun org-get-todo-sequence-tail (kwd)
    "Return the tail of the TODO sequence to which KWD belongs.
If KWD is not set, check if there is a text property remembering the
right sequence."
    (let (p)
      (cond
       ((not kwd)
        (or (get-text-property (point-at-bol) 'org-todo-head)
            (progn
              (setq p (next-single-property-change (point-at-bol) 'org-todo-head
                                                   nil (point-at-eol)))
              (get-text-property p 'org-todo-head))))
       ((not (member kwd org-todo-keywords-1))
        (car org-todo-keywords-1))
       (t (first (last (assoc kwd org-todo-kwd-alist)))))))

  (defun org-stagnant-project-p ()
    (block "fun"
      (save-restriction
        (save-excursion
          (outline-show-branches)
          (org-narrow-to-subtree)
          (while (not (eobp))
            (org-next-visible-heading 1)
            (if (string= (org-get-todo-state) "NEXT")
                (return-from "fun" nil)))
          t))))

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
           "* TODO %?%A")
          ("c" "Cliplink" entry (file +org-capture-inbox-file)
           "* TODO %(org-cliplink-capture)%?")
          ("n" "Notes" entry
           (file+headline +org-capture-notes-file "Inbox")
           "* %u\n%?")
          ("b" "Books" entry
           (file+headline +org-capture-books-file "Inbox")
           "* [ ] %?\n%^{AUTHOR}p")
          ("h" "Habits" entry
           (file+headline +org-capture-habits-file "Inbox")
           "* TODO %?\nSCHEDULED: <%<%Y-%m-%d %a> %^{repeat|+1d}>\n:PROPERTIES:\n:STYLE: habit\n:END:")))
  ;; ("r" "Run Week" entry
  ;;  (file +org-capture-running-file)
  ;;  "* Week %<%W>\n%^{Mon}%t\n%^{Tue}%t\n%^{Wed}%t"
  ;;  :prepend t :immediate-finish t)))

  (setq org-refile-targets '((nil :maxlevel . 9)
                             (org-agenda-files :maxlevel . 9)))

  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm)

  (setq org-agenda-custom-commands
        '(("n" "Next Actions"
           ((agenda nil
                    ((org-agenda-span 1)
                     (org-agenda-start-day "+0d")))
            (todo "NEXT"
                  ((org-agenda-overriding-header "Next")))
            (todo "EMAIL"
                  ((org-agenda-overriding-header "Email")))
            (todo "WAITING"
                  ((org-agenda-overriding-header "Waiting on"))))
           nil)

          ("g" "Meetings"
           ((tags "device"
                  ((org-agenda-overriding-header "Device Group")))
            (tags "big meeting"
                  ((org-agenda-overriding-header "Stats/Data Management")))
            (tags "Andrew"
                  ((org-agenda-overriding-header "Lim")))
            (tags "Jeff"
                  ((org-agenda-overriding-header "Hausdorff"))))
           nil)

          ("p" "Places"
           ((tags-todo "@office"
                       ((org-agenda-overriding-header "At Rush")))
            (tags-todo "@home"
                       ((org-agenda-overriding-header "At Home")))
            (tags-todo "@computer"
                       ((org-agenda-overriding-header "At Computer")))
            (tags-todo "@phone"
                       ((org-agenda-overriding-header "At Phone")))
            (tags-todo "@errand"
                       ((org-agenda-overriding-header "On Errands"))))
           nil))))

(after! (:and ox org)
  (setq org-latex-prefer-user-labels t)
  (setq org-latex-pdf-process (list "latexmk -g -pdf -%latex %f"))
  (add-to-list 'org-latex-classes
               '("tufte-handout" "\\documentclass{tufte-handout}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")))
  (add-to-list 'org-latex-classes
               '("article" "\\documentclass[11pt]{scrartcl}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")))
  (add-to-list 'org-latex-classes
               '("rushdoc" "\\documentclass{rushdoc}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")))
  (add-to-list 'org-latex-classes
               '("rushpres" "\\documentclass{rushpresentation}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")))

  (add-to-list 'org-latex-packages-alist
               '("" "pgfplots" nil))
  (add-to-list 'org-latex-packages-alist
               '("" "booktabs" nil))

  (load (concat doom-private-dir "extras/+ox-word.el"))
  (require 'ox-extra)
  (ox-extras-activate '(ignore-headlines))
  (setq org-export-with-toc nil))

(add-to-list 'org-file-apps '("\\.pdf\\'" . (lambda (_file link)
                                              (call-process "xdg-open" nil 0 nil link))))
(after! org-ref
  (org-ref-ivy-cite-completion)
  (setq org-ref-completion-library 'org-ref-ivy-cite)
  (setq org-ref-default-ref-type "cref")
  (setq org-ref-bibliography-notes refs-notes)
  (setq org-ref-notes-function (lambda (key)
                                 (interactive)
                                 (dc-goto-or-create-workspace "References")
                                 (org-ref-notes-function-many-files key)))

  (setq org-ref-default-bibliography (list refs-bib))
  (setq org-ref-pdf-directory refs-pdfs)
  (setq org-ref-get-pdf-filename-function
        (lambda (key)
          (let ((files (directory-files-recursively org-ref-pdf-directory
                                                    (concat key ".pdf"))))
            (if (= 1 (length files))
                (car files)
              (completing-read "Choose: " files)))))

  (defun org-ref-open-pdf-at-point (arg)
    "Open the pdf for bibtex key under point if it exists.
Redefined so pdf is opened in emacs when prefed with `\\[universal-argument]'
instead externally"
    (interactive "p")
    (let* ((results (org-ref-get-bibtex-key-and-file))
           (key (car results))
           (pdf-file (funcall org-ref-get-pdf-filename-function key)))
      (if (file-exists-p pdf-file)
          (if (= arg 1)
              (org-open-file pdf-file)
            (org-open-file pdf-file 'open-in-emacs))
        (message "no pdf found for %s" key)))))

(after! bibtex
  (require 'find-lisp)
  (setq bibtex-completion-bibliography refs-bib)
  (setq bibtex-completion-additional-search-fields '(keywords))
  (setq bibtex-completion-library-path
        (remove-if-not
         (lambda (f) (find-lisp-file-predicate-is-directory f refs-pdfs))
         (directory-files-recursively refs-pdfs "." 'dirs)))
  (setq bibtex-completion-notes-path refs-notes)
  (setq bibtex-completion-pdf-open-function
        (lambda (fpath) (call-process "xdg-open" nil 0 nil fpath)))
  (setq bibtex-completion-notes-template-multiple-files
        "${title}\n#+AUTHOR: ${author-or-editor}\n#+ROAM_KEY: ${=key=}\n"))

