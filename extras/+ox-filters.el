;;; ~/.doom.d/extras/+ox-filters.el -*- lexical-binding: t; -*-

(defun dc-org-export-convert-tikz-h (backend)
  (cond ((string= backend "hugo")
         (dc--convert-all-tikz-to "svg"))))

(defun dc--convert-all-tikz-to (ext)
  (let ((tikz-regex "\\[file:\\(.*\\)\.tikz\\]"))
    (while (re-search-forward tikz-regex nil t)
      (dc--convert-tikz (match-string 1) ext))))

(defun dc--convert-tikz (name new-ext)
  (if (or (not (file-exists-p (concat name "." new-ext)))
          (< (ow--file-modification-time (concat name "." new-ext))
             (ow--file-modification-time (concat name ".tikz"))))
      (progn (shell-command (concat "tikz2" new-ext " " name ".tikz"))
             (message (concat name ".tikz" " converted to " new-ext))))
  (replace-match (concat "[file:" name "." new-ext "]")))

(defun ow--file-modification-time (file)
  "Posix time of last modification."
  (string-to-number (format-time-string "%s"
                      (file-attribute-modification-time
                       (file-attributes file)))))

(add-hook 'org-export-before-processing-hook #'dc-org-export-convert-tikz-h)
