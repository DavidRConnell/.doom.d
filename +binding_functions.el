;;; ~/.doom.d/+binding_functions.el -*- lexical-binding: t; -*-

(evil-define-operator dc-comment-line-operator (beg end)
  "Wrapper around `evilnc comment operator' to always comments whole lines"
  :move-point nil
  :type line
  :repeat t
  :jump t
  (interactive "<r>")
  (evilnc-comment-operator beg end))

(evil-define-operator dc-apply-ex-substitute-word (beg end)
  "Apply search and replace to each line."
  :move-point nil
  :type line
  :repeat t
  :jump t
  (interactive "<r>")
  (setq beg (1+ (count-lines (point-min) beg)))
  (setq end (count-lines (point-min) end))
  (let ((prefix (format "%d,%d" beg end)))
    (evil-ex (concat prefix "s/\\<" (thing-at-point 'word) "\\>/"))))

(evil-define-operator dc-apply-ex-substitute-WORD (beg end)
  "Apply search and replace to each line."
  :move-point nil
  :type line
  :repeat t
  :jump t
  (interactive "<r>")
  (setq beg (1+ (count-lines (point-min) beg)))
  (setq end (count-lines (point-min) end))
  (let ((prefix (format "%d,%d" beg end)))
    (evil-ex (concat prefix "s/\\<" (thing-at-point 'symbol) "\\>/"))))

(evil-define-operator dc-apply-ex-substitute-custom (beg end)
  "Apply search and replace to each line."
  :move-point nil
  :type line
  :repeat t
  :jump t
  (interactive "<r>")
  (setq beg (1+ (count-lines (point-min) beg)))
  (setq end (count-lines (point-min) end))
  (let ((prefix (format "%d,%d" beg end)))
    (evil-ex (concat prefix "s/\\<"))))

(defun dc--get-evil-ex-prefix ()
  "Format universal argument for use on evil-ex command state."
  (cond
   ((and (evil-visual-state-p)
         evil-ex-visual-char-range
         (memq (evil-visual-type) '(inclusive exclusive)))
    "`<,`>")
   ((evil-visual-state-p)
    "'<,'>")
   (current-prefix-arg
    (let ((arg (prefix-numeric-value current-prefix-arg)))
      (cond ((< arg 0) (format ".%+d,." (1+ arg)))
            ((> arg 0) (format ".,.%+d" (1- arg)))
            (t "."))))))

(defun dc--get-sp-pair (prefix)
  "Get pair in format for use with sp-rewrap sexp."
  (--first (equal prefix (car it)) (sp--get-pair-list-context 'wrap)))

(defun dc-open-in-workspace (name file)
  "Open FILE in the workspace NAME creating it if it doesn't already exist.
If FILE is a directory search with `counsel-find-file'"
  (interactive)
  (if (not (+workspace-exists-p name))
      (+workspace-new name))
  (+workspace-switch name)

  (if (file-directory-p file)
      (counsel-find-file file)
    (find-file file)))

(defun dc-open-org-file ()
  "Open org file in org-directory."
  (interactive)
  (if (not (+workspace-exists-p "Org"))
      (+workspace-new "Org"))
  (+workspace-switch "Org")

  (let ((file (completing-read "Open file: "
                               (directory-files org-directory)
                               #'(lambda (f) (string= (file-name-extension f) "org")))))
    (find-file (concat org-directory file))))

(defun dc-find-file-on-server ()
  (interactive)
  (if (not (+workspace-exists-p "Server"))
      (+workspace-new "Server"))
  (+workspace-switch "Server")

  (counsel-find-file "/ssh:"))

(defun dc-find-external-manual (man-dir)
  (interactive)
  (let ((man (completing-read "Find manual: "
                             (directory-files man-dir))))
    (info-setup (concat man-dir "/" man "/" man ".info")
                (pop-to-buffer "*info*"))))
