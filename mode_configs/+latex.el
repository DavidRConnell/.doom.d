;;; ~/.doom.d/mode_configs/+latex.el -*- lexical-binding: t; -*-

(after! auctex
  (lambda! (TeX-engine-set 'xetex)))

(add-hook! 'LaTeX-mode-hook
           #'outline-minor-mode)

(defun dc-insert-latex-figure ()
  "Insert new figure"
  (interactive)
  (let* ((figures (strip-extensions (dc-latex-list-figures "./figures")))
         (default (car figures))
         (figure (completing-read (concat "Figure (default "
                                               default "): ")
                                  figures)))

    (forward-line)
    (open-line 1)
    (insert (concat "\n\\inputfigure{" figure "}"))))

(defun dc-latex-list-figures (dir)
  (let ((figure-extensions '("tex" "pgf" "svg" "png" "jpeg")))
    (remove-if-not (lambda (x) (member (file-name-extension x) figure-extensions))
                   (directory-files dir))))

(defun strip-extensions (file-list)
    (cl-loop for file-name in file-list
             collect (file-name-sans-extension file-name)))

(defun dc-insert-latex-table ()
  "Insert new table"
  (interactive)
  (let* ((tables (strip-extensions (dc-latex-list-tex-files "./tables")))
         (default (car tables))
         (table (completing-read (concat "Table (default "
                                               default "): ")
                                  tables)))

    (forward-line)
    (open-line 1)
    (insert (concat "\n\\inputtable{" table "}"))))

(defun dc-latex-list-tex-files (dir)
  (let ((tex-extensions '("tex")))
    (remove-if-not (lambda (x) (member (file-extension x) tex-extensions))
                   (directory-files dir))))

(defun dc-insert-latex-section ()
  "Insert new section"
  (interactive)
  (let* ((sections (strip-extensions (dc-latex-list-tex-files "./sections")))
         (default (car sections))
         (section (completing-read (concat
                                    "Section name (default " default "): ")
                                   sections)))

    (if (not (member section sections))
        (let ((file-name (concat section ".tex")))
          (save-excursion
            (find-file (concat "./sections/" file-name))
            (write-file file-name nil)
            (kill-buffer))))

    (forward-line)
    (open-line 1)
    (insert (concat "\\inputsection{" section "}"))))
