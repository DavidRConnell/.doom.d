;;; +pdf.el --- description -*- lexical-binding: t; -*-

(after! pdf-tools
  (map! :mode pdf-view-mode
        :ni "i" #'org-noter-insert-note
        :ni "r" #'org-ref-pdf-to-bibtex
        :ni "j" #'pdf-view-next-line-or-next-page
        :ni "k" #'pdf-view-previous-line-or-previous-page
        :ni "C-j" #'pdf-view-next-page
        :ni "C-k" #'pdf-view-previous-page
        :ni "/" #'pdf-occur
        (:localleader
          "l" #'dc-pdf-outline-imenu-slim))

  (map! :map pdf-occur-buffer-mode-map
        :ni "C-n" #'evil-next-visual-line
        :ni "C-p" #'evil-previous-visual-line))

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
