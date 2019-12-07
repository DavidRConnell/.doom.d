(after! matlab
  (map! :mode matlab-mode
        (:localleader
          "?" (lambda! (matlab-shell-describe-command
                        (matlab-read-word-at-point)))
          "d" (lambda! (matlab-find-file-on-path
                        (matlab-read-word-at-point)))
          "R" #'matlab-run-command
          "r" #'matlab-run-last-command
          "c" #'matlab-close-figures
          "e" #'mlint-show-warning
          "l" #'matlab-shell-apropos
          "p" #'matlab-shell)
        (:prefix "C-c"
          "C-c" #'matlab-shell-run-cell
          "C-l" #'matlab-shell-run-region-or-line)))

(after! matlab
  (defun matlab-run-last-command ()
    "Run the last command sent to the matlab shell buffer."
    (interactive)
    (let ((curr-buffer (buffer-name)))
      (switch-to-buffer (concat "*" matlab-shell-buffer-name "*"))
      (matlab-shell-send-string
       (concat
        (comint-previous-input-string 0) "\n"))
      (switch-to-buffer curr-buffer)))

  (defun matlab-run-command ()
    "Send a command to the running matlab shell buffer."
    (interactive)
    (let ((curr-buffer (buffer-name))
          (cmd (read-from-minibuffer "Command: ")))
      (switch-to-buffer (concat "*" matlab-shell-buffer-name "*"))
      (matlab-shell-send-string (concat cmd "\n"))
      (matlab-shell-add-to-input-history cmd)
      (switch-to-buffer curr-buffer)))

  (defun matlab-close-figures ()
    "Pass the matlab shell function FUN with optional arguments ARGS to a matlab shell."
    (interactive)
    (let ((curr-buffer (buffer-name)))
      (switch-to-buffer (concat "*" matlab-shell-buffer-name "*"))
      (matlab-shell-close-figures)
      (switch-to-buffer curr-buffer))))
