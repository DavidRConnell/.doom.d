(defun turn-off-flycheck ()
  (flycheck-mode -1))

(setq org-directory "~/org/")
(setq org-agenda-files (list "~/org/"))

(after! org
  ;; (require 'org-ref)
  (require 'org-drill)

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
                    "STAGNANT(s)"
                    "WAITING(w@)"
                    "|" "DONE(d!/)")
          (sequence "MAYBE(m)" "LATER(l)" "|" "CANCELLED(c)")
          (type "NOTES(N)")
          (sequence "[ ]([)" "[-](-!/)" "|" "[x](x!/)" "[/](/)")))

  (setq org-todo-keyword-faces
        '(("TODO" . (:foreground "#98be65" :weight bold))
          ("NEXT" . (:foreground "#da8548" :weight bold))
          ("EMAIL" . (:foreground "#da8548" :weight bold))
          ("MAYBE" . (:foreground "#ecbe7b" :weight bold))
          ("WAITING" . (:foreground "#46d9ff" :weight bold))
          ("LATER" . (:foreground "#46d9ff" :weight bold))
          ("STAGNANT" . (:foreground "#bf5150" :weight bold))
          ("NOTES" . (:foreground "#d2691e" :weight bold))
          ("QUESTION" . (:foreground "#bd93f9" :weight bold))
          ("FILE" . (:foreground "#ecbe7b" :weight bold))
          ("INPROGRESS" . (:foreground "#bd93f9" :weight bold))
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
  (setq org-latex-pdf-process (list "latexmk --shell-escape -g -pdf -%latex %f"))
  (setq org-latex-tables-booktabs t)
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
               '("newfloat" "minted" nil))
  (add-to-list 'org-latex-packages-alist
               '("" "booktabs" nil))

  (load (concat doom-private-dir "extras/+ox-word.el"))
  (add-to-list 'org-latex-listings-langs '(matlab "matlab"))
  (setq org-latex-listings 'minted)
  (setq org-latex-minted-options '(("frame" "lines") ("framesep" "3mm")
                                   ("fontsize" "\\small") ("bgcolor" "mintedbg")))
  (setq org-latex-to-mathml-convert-command
        "latexmlmath \"%i\" --presentationmathml=%o")

  (require 'ox-extra)
  (ox-extras-activate '(ignore-headlines))
  (setq org-export-with-toc nil))

(after! ob-octave
  (load (concat doom-private-dir "extras/+ob-octave-fix.el")))

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

  (defun dc-open-pdf-at-point (arg)
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
        "${title}\n#+AUTHOR: ${author-or-editor}\ncite:${=key=}"))

(after! ivy-bibtex
  (advice-add
   #'ivy-bibtex-edit-notes :before (lambda (_) (dc-goto-or-create-workspace "References"))))

(setq org-roam-directory zettle-dir)
(after! (:and org org-roam)
  (setq org-roam-graph-viewer "/usr/bin/qutebrowser")
  (setq org-roam-capture-templates
        '(("d" "default" plain #'org-roam-capture--get-point
           "%?"
           :file-name "%<%Y%m%d%H%M%S>"
           :head "#+TITLE: ${title}\n"
           :unnarrowed t))))

(after! org-drill
  (setq org-drill-add-random-noise-to-intervals-p t))

(after! (:and org-ref ox-hugo)
  (require 'org-ref-ox-hugo)
  (add-to-list 'org-ref-formatted-citation-formats
               '("md"
                 ("article" . "${author}, *${title}*, ${journal}, *${volume}(${number})*, ${pages} (${year}). ${doi}")
                 ("inproceedings" . "${author}, *${title}*, In ${editor}, ${booktitle} (pp. ${pages}) (${year}). ${address}: ${publisher}.")
                 ("book" . "${author}, *${title}* (${year}), ${address}: ${publisher}.")
                 ("phdthesis" . "${author}, *${title}* (Doctoral dissertation) (${year}). ${school}, ${address}.")
                 ("inbook" . "${author}, *${title}*, In ${editor} (Eds.), ${booktitle} (pp. ${pages}) (${year}). ${address}: ${publisher}.")
                 ("incollection" . "${author}, *${title}*, In ${editor} (Eds.), ${booktitle} (pp. ${pages}) (${year}). ${address}: ${publisher}.")
                 ("proceedings" . "${editor} (Eds.), _${booktitle}_ (${year}). ${address}: ${publisher}.")
                 ("unpublished" . "${author}, *${title}* (${year}). Unpublished manuscript.")
                 ("misc" . "${author} (${year}). *${title}*. Retrieved from [${howpublished}](${howpublished}). ${note}.")
                 (nil . "${author}, *${title}* (${year}).")))

  (setq org-hugo-use-code-for-kbd t)
  (setq org-blackfriday--org-element-string '((src-block . "Listing")
                                              (table . "Table")
                                              (figure . "Figure")))

  (setq org-hugo-link-desc-insert-type t)
  (defun org-ref-cref-export (keyword desc format)
    "cref link export function.
See https://www.ctan.org/tex-archive/macros/latex/contrib/cleveref"
    (cond
     ((eq format 'latex) (format "\\cref{%s}" keyword))
     ;; considering the fact that latex's the standard of math formulas, just use
     ;;mathjax to render the html customize the variable
     ;;'org-html-mathjax-template' and 'org-html-mathjax-options' referring to
     ;;'autonumber'
     ((or (eq format 'md) (eq format 'html))
      (let (type)
        (when (string-match "\\(.*\\):.*" keyword)
          (setq type (match-string 1 keyword))
          (cond
           ((string= type "eq")
            (format "eq \\ref{%s}" keyword))))))))

(defun org-hugo-link (link desc info)
    "Convert LINK to Markdown format.

DESC is the link's description.
INFO is a plist used as a communication channel.

Unlike `org-md-link', this function will also copy local images
and rewrite link paths to make blogging more seamless."
    (let* ((raw-link (org-element-property :raw-link link))
           (raw-path (org-element-property :path link))
           (type (org-element-property :type link))
           (link-is-url (member type '("http" "https" "ftp" "mailto"))))
      (when (and (stringp raw-path)
                 link-is-url)
        (setq raw-path (org-blackfriday--url-sanitize
                        (url-encode-url raw-path))))
      ;; (message "[ox-hugo-link DBG] link: %S" link)
      ;; (message "[ox-hugo-link DBG] link path: %s" (org-element-property :path link))
      ;; (message "[ox-hugo-link DBG] link filename: %s" (expand-file-name (plist-get (car (cdr link)) :path)))
      ;; (message "[ox-hugo-link DBG] link type: %s" type)
      (cond
       ;; Link type is handled by a special function.
       ((org-export-custom-protocol-maybe link desc 'md))
       ((member type '("custom-id" "id" "fuzzy" "cref"))
        (let ((destination (if (member type '("fuzzy" "cref"))
                               (org-export-resolve-fuzzy-link link info)
                             (org-export-resolve-id-link link info))))
          ;; (message "[org-hugo-link DBG] link destination elem type: %S" (org-element-type destination))
          (pcase (org-element-type destination)
            (`plain-text                  ;External file
             (let ((path (progn
                           ;; Treat links to `file.org' as links to `file.md'.
                           (if (string= ".org" (downcase (file-name-extension destination ".")))
                               (concat (file-name-sans-extension destination) ".md")
                             destination))))
               (if desc
                   (format "[%s](%s)" desc path)
                 (format "<%s>" path))))
            (`headline                 ;Links of type [[* Some heading]]
             (let ((title (org-export-data (org-element-property :title destination) info)))
               (format
                "[%s](#%s)"
                ;; Description
                (cond ((org-string-nw-p desc))
                      ((org-export-numbered-headline-p destination info)
                       (mapconcat #'number-to-string
                                  (org-export-get-headline-number destination info)
                                  "."))
                      (t
                       title))
                ;; Reference
                (org-hugo--get-anchor destination info title))))
            (_
             (let ((description
                    (or (org-string-nw-p desc)
                        ;; TODO: Need to get this to inc figure with subfigure type
                        (let ((number (org-export-get-ordinal
                                       destination info
                                       nil #'org-html--has-caption-p)))
                          (when number
                            (let ((num-str (if (atom number)
                                               (number-to-string number)
                                             (mapconcat #'number-to-string number "."))))
                              ;; (message "[ox-hugo-link DBG] num-str: %s" num-str)
                              (if org-hugo-link-desc-insert-type
                                  (let* ((type (org-element-type destination))
                                         ;; Org doesn't have a specific
                                         ;; element for figures. So if
                                         ;; the element is `paragraph',
                                         ;; and as this element has an
                                         ;; ordinal, we will assume that
                                         ;; to be a figure.
                                         (type (if (equal 'paragraph type)
                                                   'figure
                                                 type))
                                         (type-str (org-blackfriday--translate type info)))
                                    (format "%s %s" type-str num-str))
                                num-str)))))))
               ;; (message "[ox-hugo-link DBG] link description: %s" description)
               (when description
                 (format "[%s](#%s)"
                         description
                         (if (memq (org-element-type destination) '(src-block table))
                             (org-blackfriday--get-reference destination)
                           (org-export-get-reference destination info)))))))))
       ((org-export-inline-image-p link org-html-inline-image-rules)
        ;; TODO: Add subfigure stuff here
        ;; (message "[org-hugo-link DBG] processing an image: %s" desc)
        (let* ((parent (org-export-get-parent link))
               (parent-type (org-element-type parent))
               ;; If this is a hyper-linked image, it's parent type will
               ;; be a link too. Get the parent of *that* link in that
               ;; case.
               (grand-parent (when (eq parent-type 'link)
                               (org-export-get-parent parent)))
               (useful-parent (if grand-parent
                                  grand-parent
                                parent))
               (attr (org-export-read-attribute :attr_html useful-parent))
               (caption (or
                         ;; Caption set using #+caption takes higher precedence.
                         (org-string-nw-p
                          (org-export-data  ;Look for caption set using #+caption
                           (org-export-get-caption (org-export-get-parent-element link))
                           info))
                         (plist-get attr :caption)))
               (caption (when (org-string-nw-p caption)
                          (format "%s%s%s%s"
                                  ;; Tue Feb 13 11:32:45 EST 2018 - kmodi
                                  ;; Add the span tag once
                                  ;; https://github.com/gohugoio/hugo/issues/4406
                                  ;; gets resolved.
                                  "" ;"<span class=\\\"figure-number\\\">"
                                  (format (org-html--translate
                                           (concat
                                            (cdr (assoc 'figure org-blackfriday--org-element-string))
                                            " %d:")
                                           info)
                                          (org-export-get-ordinal
                                           useful-parent info
                                           nil #'org-html--has-caption-p))
                                  " "     ;" </span>"
                                  ;; Escape the double-quotes, if any.
                                  (replace-regexp-in-string "\"" "\\\\\"" caption))))
               (extension (downcase (file-name-extension raw-path)))
               (inlined-svg (and (stringp extension)
                                 (string= "svg" extension)
                                 (plist-get attr :inlined))))
          ;; (message "[ox-hugo-link DBG] Inline image: %s, extension: %s" raw-path extension)
          ;; (message "[ox-hugo-link DBG] inlined svg? %S" inlined-svg)
          ;; (message "[ox-hugo-link DBG] caption: %s" caption)
          (if inlined-svg
              (let* ((svg-contents (with-temp-buffer
                                     (insert-file-contents raw-path)
                                     (fill-region (point-min) (point-max)) ;Make huge one-liner SVGs sane
                                     (buffer-substring-no-properties (point-min) (point-max))))
                     (svg-contents-sanitized (replace-regexp-in-string
                                              ;; Remove the HTML comments.
                                              "<!--\\(.\\|\n\\)*?-->" ""
                                              (replace-regexp-in-string
                                               ;; Remove the xml document tag as that cannot be inlined in-between
                                               ;; a Markdown (or even an HTML) file.
                                               "<\\?xml version=\"1\\.0\" encoding=\"UTF-8\" standalone=\"no\"\\?>" ""
                                               svg-contents)))
                     (caption-html (if (not caption)
                                       ""
                                     (format (concat "\n\n<div class=\"figure-caption\">\n"
                                                     "  %s\n"
                                                     "</div>")
                                             (org-html-convert-special-strings ;Interpret em-dash, en-dash, etc.
                                              (org-export-data-with-backend caption 'html info))))))
                ;; (message "[ox-hugo-link DBG] svg contents: %s" svg-contents)
                ;; (message "[ox-hugo-link DBG] svg contents sanitized: %s" svg-contents-sanitized)
                (concat svg-contents-sanitized caption-html))
            (let* ((path (org-hugo--attachment-rewrite-maybe raw-path info))
                   (inline-image (not (org-html-standalone-image-p useful-parent info)))
                   (source (if link-is-url
                               (concat type ":" path)
                             path))
                   (num-attr (/ (length attr) 2)) ;(:alt foo) -> num-attr = 1
                   (alt-text (plist-get attr :alt)))
              ;; (message "[ox-hugo-link DBG] path: %s" path)
              ;; (message "[ox-hugo-link DBG] inline image? %s" inline-image)
              ;; (message "[org-hugo-link DBG] attr: %s num of attr: %d"
              ;;          attr (length attr))
              ;; (message "[org-hugo-link DBG] parent-type: %s" parent-type)
              ;; (message "[org-hugo-link DBG] useful-parent-type: %s"
              ;;          (org-element-type useful-parent))
              (cond
               (;; Use the Markdown image syntax if the image is inline and
                ;; there are no HTML attributes for the image, or just one
                ;; attribute, the `alt-text'.
                (and inline-image
                     (or (= 0 num-attr)
                         (and alt-text
                              (= 1 num-attr))))
                (let ((alt-text (if alt-text
                                    alt-text
                                  "")))
                  (format "![%s](%s)" alt-text source)))
               (;; Else if the image is inline (with non-alt-text
                ;; attributes), use HTML <img> tag syntax.
                inline-image
                ;; The "target" and "rel" attributes would be meant for <a>
                ;; tags. So do not pass them to the <img> tag.
                (plist-put attr :target nil)
                (plist-put attr :rel nil)
                (org-html--format-image source attr info))
               (t ;Else use the Hugo `figure' shortcode.
                ;; Hugo `figure' shortcode named parameters.
                ;; https://gohugo.io/content-management/shortcodes/#figure
                (let ((figure-params `((src . ,source)
                                       (alt . ,alt-text)
                                       (caption . ,caption)
                                       (link . ,(plist-get attr :link))
                                       (title . ,(plist-get attr :title))
                                       (class . ,(plist-get attr :class))
                                       (attr . ,(plist-get attr :attr))
                                       (attrlink . ,(plist-get attr :attrlink))
                                       (width . ,(plist-get attr :width))
                                       (height . ,(plist-get attr :height))
                                       ;; While the `target' and `rel'
                                       ;; attributes are not supported by
                                       ;; the inbuilt Hugo `figure'
                                       ;; shortcode, they can be used as
                                       ;; intended if a user has a custom
                                       ;; `figure' shortcode with the
                                       ;; support added for those.
                                       (target . ,(plist-get attr :target))
                                       (rel . ,(plist-get attr :rel))))
                      (figure-param-str ""))
                  (dolist (param figure-params)
                    (let ((name (car param))
                          (val (cdr param)))
                      (when val
                        (setq figure-param-str (concat figure-param-str
                                                       (format "%s=\"%s\" "
                                                               name val))))))
                  ;; (message "[org-hugo-link DBG] figure params: %s" figure-param-str)
                  (format "{{< figure %s >}}" (org-trim figure-param-str)))))))))
       ((string= type "coderef")
        (let ((ref (org-element-property :path link)))
          (format (org-export-get-coderef-format ref desc)
                  (org-export-resolve-coderef ref info))))
       ((equal type "radio")
        (let ((destination (org-export-resolve-radio-link link info)))
          (format "[%s](#%s)" desc (org-export-get-reference destination info))))
       (t
        (let* ((link-param-str "")
               (path (cond
                      (link-is-url
                       ;; Taken from ox-html.el -- Extract attributes
                       ;; from parent's paragraph.  HACK: Only do this
                       ;; for the first link in parent (inner image link
                       ;; for inline images).  This is needed as long as
                       ;; attributes cannot be set on a per link basis.
                       (let* ((attr
                               (let ((parent (org-export-get-parent-element link)))
                                 (and (eq (org-element-map parent 'link #'identity info :first-match) link)
                                      (org-export-read-attribute :attr_html parent))))
                              ;; https://www.w3schools.com/tags/tag_link.asp
                              (link-params `((media . ,(plist-get attr :media))
                                             (target . ,(plist-get attr :target))
                                             (rel . ,(plist-get attr :rel))
                                             (sizes . ,(plist-get attr :sizes))
                                             (type . ,(plist-get attr :type)))))
                         (dolist (param link-params)
                           (let ((name (car param))
                                 (val (cdr param)))
                             (when val
                               (setq link-param-str (concat link-param-str
                                                            (format "%s=\"%s\" "
                                                                    name val))))))
                         ;; (message "[ox-hugo-link DBG] link params: %s" link-param-str)
                         )
                       (concat type ":" raw-path))
                      (;; Remove the "file://" prefix.
                       (string= type "file")
                       ;; (message "[ox-hugo-link DBG] raw-path: %s" raw-path)
                       (let ((path1 (replace-regexp-in-string "\\`file://" "" raw-path)))
                         (if (string= ".org" (downcase (file-name-extension path1 ".")))
                             (let ((raw-link-minus-org-file
                                    ;; If raw-link is "./foo.org::#bar",
                                    ;; set `raw-link-minus-org-file' to
                                    ;; "#bar".
                                    (if (string-match ".*\\.org::\\(#.*\\)" raw-link)
                                        (match-string-no-properties 1 raw-link)
                                      "")))
                               (format "{{< relref \"%s%s\" >}}"
                                       (file-name-sans-extension
                                        (file-name-nondirectory path1))
                                       raw-link-minus-org-file))
                           (org-hugo--attachment-rewrite-maybe path1 info))))
                      (t
                       raw-path)))
               (link-param-str (org-string-nw-p (org-trim link-param-str))))
          ;; (message "[ox-hugo-link DBG] desc=%s path=%s" desc path)
          ;; (message "[ox-hugo-link DBG] link-param-str=%s" link-param-str)
          (cond
           ;; Link description is a `figure' shortcode but does not
           ;; already have the `link' parameter set.
           ((and desc
                 (string-match-p "\\`{{<\\s-*figure\\s-+" desc)
                 (not (string-match-p "\\`{{<\\s-*figure\\s-+.*link=" desc)))
            (replace-regexp-in-string "\\s-*>}}\\'"
                                      (format " link=\"%s\"\\&" path)
                                      desc))
           ;; Both link description and link attributes are present.
           ((and desc
                 link-param-str)
            (format "<a href=\"%s\" %s>%s</a>"
                    (org-html-encode-plain-text path)
                    link-param-str
                    (org-link-unescape desc)))
           ;; Only link description, but no link attributes.
           (desc
            (format "[%s](%s)" desc path))
           ;; Only link attributes, but no link description.
           (link-param-str
            (let ((path (org-html-encode-plain-text path)))
              (format "<a href=\"%s\" %s>%s</a>"
                      path
                      link-param-str
                      (org-link-unescape path))))
           ;; Neither link description, nor link attributes.
           (t
            (if (string-prefix-p "{{< relref " path)
                (format "[%s](%s)" path path)
              (format "<%s>" path))))))))))
