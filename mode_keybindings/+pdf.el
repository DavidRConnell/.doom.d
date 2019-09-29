;;; +pdf.el --- description -*- lexical-binding: t; -*-

(map! :mode pdf-view-mode
      :ni "n" #'org-noter
      :ni "i" #'org-noter-insert-note
      :ni "r" #'org-ref-pdf-to-bibtex)

(provide '+pdf)
;;; +pdf.el ends here
