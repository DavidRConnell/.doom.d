;;; ~/.doom.d/+binding_functions.el -*- lexical-binding: t; -*-

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
    (evil-ex (concat prefix "s/"))))

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

(defun dc-goto-or-create-workspace (name)
  "Go to workspace NAME; if it doesn't exist already create it first."
  (if (not (+workspace-exists-p name))
           (+workspace-new name))
  (+workspace-switch name))

(defun dc-open-in-workspace (name file)
  "Open FILE in the workspace NAME creating it if it doesn't already exist.
If FILE is a directory search with `counsel-find-file'"
  (interactive)
  (dc-goto-or-create-workspace name)

  (if (file-directory-p file)
      (projectile-find-file-in-directory file)
    (find-file file)))

(defun dc-run-deft-in-workspace (name directory)
  "Run deft over DIRECTORY in workspace NAME, creating it if necessary"
  (progn (dc-goto-or-create-workspace name)
         (setq deft-directory directory)
         (call-interactively #'deft)))

(defun dc-open-org-file-in-workspace (name directory)
  "Open org file from DIRECTORY in workspace NAME, creating it if necessary."
  (interactive)
  (dc-goto-or-create-workspace name)

  (let ((file (completing-read "Open file: "
                               (directory-files directory)
                               #'(lambda (f) (string= (file-name-extension f) "org")))))
    (find-file (concat directory file))))

(defun dc-find-file-on-server ()
  (interactive)
  (dc-goto-or-create-workspace "Server")

  (counsel-find-file "/ssh:"))

(defun dc-find-external-manual (man-dir)
  (interactive)
  (let ((man (completing-read "Find manual: "
                             (directory-files man-dir))))
    (info-setup (concat man-dir "/" man "/" man ".info")
                (pop-to-buffer "*info*"))))

(defun dc-project-help (parent-dir)
  (interactive)
  (dc-goto-or-create-workspace "Package")
  (let* ((project (completing-read "Project: "
                                   (directory-files parent-dir)))
         (file-base (concat parent-dir project "/README")))
    (cond ((file-exists-p (concat file-base ".org"))
           (find-file (concat file-base ".org")))
          ((file-exists-p (concat file-base ".md"))
           (find-file (concat file-base ".md")))
          ((file-exists-p (concat parent-dir project "/" project ".el"))
           (find-file (concat parent-dir project "/" project ".el")))
          (t (message "Could not find README file")))))

(defmacro dc-arg-cmd (func1 func2 &optional func3)
  "Select a function to call based on the number of \\[universal-argument]

If \\[universal-argument] not hit run FUNC1, one \\[universal-argument] selects
  FUNC2, two \\[universal-argument] selects FUNC3 if it exists.
If there no FUNC3 is provided defaults to FUNC2."

  `(lambda (arg)
    (interactive "p")
    (cond ((and ,func3 (> arg 4))
           (call-interactively ,func3))
          ((>= arg 4)
           (call-interactively ,func2))
          (t
           (call-interactively ,func1)))))

(defmacro dc--select-function-with-universal-arg (name func1 func2)
  "Select a function to call based on the number of \\[universal-argument]

If \\[universal-argument] not hit run FUNC1, one \\[universal-argument] selects
  FUNC2, two \\[universal-argument] selects FUNC3 if it exists.
If there no FUNC3 is provided defaults to FUNC2."

  (declare (obsolete dc-arg-cmd "Wed May 6 19:19:22 2020"))
  `(defun ,name (arg)
    (interactive "p")
    (cond ((= arg 1)
           (call-interactively ,func1))
          ((>= arg 4)
           (call-interactively ,func2)))))

(defun dc-plop-ref-outline ()
  (interactive)
  (require 'pdf-tools)
  (let* ((key (file-name-sans-extension (buffer-name)))
         (file (funcall org-ref-get-pdf-filename-function key))
         (depth))
    (dolist (elm (pdf-info-outline file))
      (setq depth (cdr (cl-first elm)))
      (insert (concat (make-string depth ?*) " " (cdr (cl-third elm)) "\n")))))

(after! vterm
		(push (list "send-to-popup"
            (lambda (file)
              (other-window 1)
              (find-file file)))
          vterm-eval-cmds))

(defun dc-workspace/switch-to (index)
  "Switch to a workspace at a given INDEX. A negative number will start from the
end of the workspace list. Edited from `+workspace/switch-to'"
  (interactive
   (list (if (featurep! :completion ivy)
                 (ivy-read "Switch to workspace: "
                           (+workspace-list-names)
                           :caller #'+workspace/switch-to
                           :preselect (+workspace-current-name))
               (completing-read "Switch to workspace: " (+workspace-list-names)))))
  (when (and (stringp index)
             (string-match-p "^[0-9]+$" index))
    (setq index (string-to-number index)))
  (condition-case-unless-debug ex
      (let ((names (+workspace-list-names))
            (old-name (+workspace-current-name)))
        (cond ((numberp index)
               (let ((dest (nth index names)))
                 (unless dest
                   (error "No workspace at #%s" (1+ index)))
                 (+workspace-switch dest)))
              ((stringp index)
               (+workspace-switch index t))
              (t
               (error "Not a valid index: %s" index)))
        (unless (called-interactively-p 'interactive)
          (if (equal (+workspace-current-name) old-name)
              (+workspace-message (format "Already in %s" old-name) 'warn)
            (+workspace/display))))
    ('error (+workspace-error (cadr ex) t))))

(defun dc-org-roam-add-link ()
  (interactive)
  (insert "[[roam:]]")
  (evil-backward-char 2)
  (evil-insert-state)
  (company-complete))
