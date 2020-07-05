;;; ox-word.el --- export org-mode to MS Word docx -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 David R. Connell
;;
;; Author: David R. Connell <https://github/DavidRConnell>
;; Created: May 14, 2020
;; Modified: Jul 4, 2020
;; Version: 0.0.1
;; Keywords:
;; Package-Requires: ((emacs 28.0.50) (cl-lib "0.5") (org-ref) (pandoc "2.0")
;; (pandoc-crossref))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;; Uses org-mode to latex export followed by pandoc to convert latex to docx.
;; Based off ideas from John Kitchin's ox-word.el After created the latex file,
;; converts all tikz and svg figures to pdf and numbers all figures, equations,
;; and tables as pandoc doesn't do this. Also adds a Reference section if
;; citations used.
;;
;;; Code:

(require 'org-ref)

(defun org-word-export-to-docx (&optional async subtreep visible-only body-only options)
  "Export current buffer to a word docx file via ox-latex and pandoc."
  (interactive)
  (org-latex-export-to-latex async subtreep visible-only body-only options)
  (let* ((bib-file (if (> (length (org-ref-get-bibtex-keys)) 0)
                       (expand-file-name (first (org-ref-find-bibliography)))
                     nil))
         (basename (file-name-sans-extension (buffer-file-name)))
         (tex-file (concat basename ".tex"))
         (docx-file (concat basename ".docx"))
         (pandoc-command (concat "pandoc "
                                 tex-file
                                 " --filter=pandoc-citeproc "
                                 (if bib-file
                                     (concat "--bibliography=" bib-file " "))
                                 "-o "
                                 docx-file))

         (buf (find-file-noselect tex-file)))

    (with-current-buffer buf
      (convert-file-tikz-figures-to-png)
      (ow-fix-references)
      (if bib-file
          (ow-add-reference-header))
      (save-buffer)
      (kill-buffer buf))

    (shell-command pandoc-command)))

(defun convert-file-tikz-figures-to-png ()
  "Convert all tikz figures in file to png."

  (let* ((tikz-regex "\\\\input{\\(.*?\\).tikz}")
         (figure-regex (concat "\\(\\\\resizebox{\\(.*?\\)}{!}{" tikz-regex "}\\)"
                               "\\|"
                               "\\(" tikz-regex "\\)"))
         (png-width "[width=%s]")
         (png-command "\\\\includegraphics%s{%s.png}"))

    (goto-char (point-min))
    (while (re-search-forward figure-regex nil t)
      (let ((basename (if (match-string 1)
                          (match-string 3)
                        (match-string 5)))
            (size (if (match-string 1)
                      (format png-width
                              (replace-regexp-in-string "\\\\" "\\\\\\\\" (match-string 2)))
                    "")))

        (replace-match (format png-command
                               size
                               basename))

        (if (or (not (file-exists-p (concat basename ".png")))
                (< (ow-file-modification-time (concat basename ".png"))
                   (ow-file-modification-time (concat basename ".tikz"))))

            (progn
              (ow-tikz2png basename)
              (message (concat basename ".tikz converted to png"))))))))

(defun ow-file-modification-time (file)
  "Posix time of last modification."
  (string-to-number (format-time-string "%s"
                      (file-attribute-modification-time
                       (file-attributes file)))))

(defun ow-tikz2png (basename)
  "Convert tikz figure FILENAME to a png file."
  (let ((textemplate (concat
                      "\\documentclass[preview,border=4mm,convert={density=600,outfile=%1$s.png}]{standalone}\n"
                      "\\usepackage{pgfplots}\n"
                      "\\pgfplotsset{compat=1.16}\n"
                      "\\usepackage{graphicx}\n"
                      "\\usepackage{tikz}\n"
                      "\\usepackage{url}\n"
                      "\\usepackage{xcolor}\n"
                      "\\begin{document}\n"
                      "\\input{%1$s.tikz}\n"
                      "\\end{document}"))
        (command "latex --shell-escape"))
    (with-temp-buffer
      (insert (format textemplate basename))
      (write-file "temp.tex"))
    (shell-command (concat command " temp.tex"))
    (dolist (f (directory-files "." nil "temp\..*"))
      (delete-file f))))

(defun ow-fix-references ()
  (let ((ref-regex "\\\\cref{\\(.*?\\):\\(.*?\\)}")
        type)
    (goto-char (point-min))
    (while (re-search-forward ref-regex nil t)
      (setq type (cond
                  ((string= (match-string 1) "fig")
                   "Figure ")
                  ((string= (match-string 1) "sec")
                   "Section ")
                  ((string= (match-string 1) "tab")
                   "Table ")
                  ((string= (match-string 1) "eq")
                   "eq ")))

      (replace-match (concat type
                             "\\\\ref{" (match-string 1) ":" (match-string 2) "}")))))

(defun ow-add-reference-header ()
  (goto-char (point-max))
  (re-search-backward "\\\\bibliography{.*?}" nil t)
  (if (match-string 0)
      ;; extra \\ needed because replace-match wants for but match-string returns only 2
      (replace-match (concat "\\\\section{References}\n\\" (match-string 0)))))

(org-export-define-derived-backend 'MSWord 'latex
  :menu-entry
  '(?w "Export to MS Word"
       ((?p "via Pandoc/LaTeX" org-word-export-to-docx))))

(provide 'ox-word)

;;; ox-word.el ends here
