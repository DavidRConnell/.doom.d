(after! matlab
  (map! :mode matlab-mode
        (:localleader
          "?" (lambda! (matlab-shell-describe-command
                  (matlab-read-word-at-point)))
          "." (lambda! (matlab-shell-locate-fcn
                  (matlab-read-word-at-point)))
          "R" #'dc-matlab-run-command
          "r" #'dc-matlab-run-last-command
          "c" #'dc-matlab-close-figures
          "l" #'matlab-shell-apropos
          "p" #'matlab-shell
          (:prefix ("t" . "test")
            "g" #'dc-matlab-toggle-test-file))
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
  (defun dc-matlab-run-last-command ()
    "Run the last command sent to the matlab shell buffer."
    (interactive)
      (dc-matlab-run-command
       (concat (comint-previous-input-string 0) "\n")))

  (defun dc-matlab-run-command (command)
    "Send a command to the running matlab shell buffer."
    (interactive "MCommand: ")
    (let ((curr-buffer (buffer-name)))
      (switch-to-buffer (concat "*" matlab-shell-buffer-name "*"))
      (matlab-shell-send-string (concat command "\n"))
      (matlab-shell-add-to-input-history command)
      (switch-to-buffer curr-buffer)))

  (defun dc-matlab-close-figures ()
    "Pass the matlab shell function FUN with optional arguments ARGS to a matlab shell."
    (interactive)
    (let ((curr-buffer (buffer-name)))
      (switch-to-buffer (concat "*" matlab-shell-buffer-name "*"))
      (matlab-shell-close-figures)
      (switch-to-buffer curr-buffer)))

  (defun dc-matlab-toggle-test-file ()
    (interactive)
    (if (string-match-p "Test" (buffer-name))
        (dc--matlab-find-original-file (projectile-project-root)
                                       (dc--matlab-get-file-name))
        (dc--matlab-find-test-file)))

  (defun dc--matlab-get-test-dir ()
    (concat (projectile-project-root) "tests/"))

  (defun dc--matlab-get-file-name (&optional test)
    (let ((buffer (buffer-name)))
      (string-match "\\([a-zA-Z0-9]*?\\)\\(Test\\)?*\\(\\.[a-zA-Z0-9]*\\)" buffer)
      (if test
          (concat (match-string 1 buffer) "Test" (match-string 3 buffer))
        (concat (match-string 1 buffer) (match-string 3 buffer)))))

  (defun dc--matlab-find-original-file (dir filename)
    (if (file-exists-p (concat dir filename))
        (find-file (concat dir filename))
      (cl-loop for f in (directory-files dir) do
               (message f)
               (if (not (string-match-p "\\." f))
                   (dc--matlab-find-original-file
                    (concat dir f "/") filename)))))

  (defun dc--matlab-find-test-file ()
    (let* ((test-dir (dc--matlab-get-test-dir))
           (test-file-name (concat test-dir
                                   (dc--matlab-get-file-name 'test))))
      (if (not (file-exists-p test-dir))
          (mkdir test-dir))

      (if (not (file-exists-p test-file-name))
          (with-temp-buffer
            (dc--matlab-insert-test-snippet (dc--matlab-get-file-name))
            (write-file test-file-name)
            (goto-char 0)))
      (find-file test-file-name)))

  (defun dc--matlab-insert-test-snippet (filename)
    "Add snippet for matlab function when opening a new .m file."
    (insert (concat "classdef " (substring filename 0 -2) "Test"
                    " < matlab.unittest.TestCase\n\n"
                    "\tmethods (TestMethodSetup)\n"
                    "\tend\n\n"
                    "\tmethods (Test, TestTags = {'Unit'})\n"
                    "\tend\n\n"
                    "\tmethods (Test, TestTags = {'Performance'})\n"
                    "\tend\n"
                    "end")))
