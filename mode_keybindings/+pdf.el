;;; +pdf.el --- description -*- lexical-binding: t; -*-

(map! :map pdf-view-mode-map
      :ni "n" #'org-noter
      :ni "i" #'org-noter-insert-note
      :ni "r" #'org-ref-pdf-to-bibtex
      :ni "j" #'pdf-view-next-line-or-next-page
      :ni "k" #'pdf-view-previous-line-or-previous-page)

(after! pdf-tools
  (defun dc-pdf-outline-imenu-slim ()
    (interactive)
    (let* ((outline (pdf-outline-imenu-create-index-flat))
           (key (completing-read
                 "Outline: "
                 (mapcar 'car outline)
                 nil t nil nil)))
      (imenu (assoc key outline)))))
(provide '+pdf)
;;; +pdf.el ends here
