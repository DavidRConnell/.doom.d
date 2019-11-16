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
