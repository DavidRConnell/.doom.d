(after! matlab
  (map! :mode matlab-mode
        (:localleader
          "?" (lambda! (matlab-shell-describe-command
                        (matlab-read-word-at-point)))
          "R" #'matlab-run-command
          "r" #'matlab-run-last-command
          "c" #'matlab-close-figures
          "l" #'matlab-shell-apropos
          "p" #'matlab-shell)
        (:prefix "C-c"
          "C-c" #'matlab-shell-run-cell
          "C-l" #'matlab-shell-run-region-or-line))

  (add-hook! 'matlab-shell-mode-hook
    (map! :mode matlab-shell-mode
          :n "k" #'matlab-shell-previous-matching-input-from-input
          :n "j" #'matlab-shell-next-matching-input-from-input
          :n "C-p" #'matlab-shell-previous-matching-input-from-input
          :n "C-n" #'matlab-shell-next-matching-input-from-input
          :ni "C-d" #'matlab-shell-exit
          :i "C-SPC" #'matlab-shell-tab)))

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
