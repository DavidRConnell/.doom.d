(after! matlab
  (map! :mode matlab-mode
        (:localleader
          :desc "Help" "?"            (lambda (arg)
                                        (interactive "P")
                                        (if arg
                                            (call-interactively
                                             #'matlab-shell-describe-command)
                                          (matlab-shell-describe-command
                                         (matlab-read-word-at-point))))
          :desc "Find function" "."   (lambda (arg)
                                        (interactive "P")
                                        (if arg
                                            (call-interactively
                                             #'matlab-shell-locate-fcn)
                                          (matlab-shell-locate-fcn
                                           (matlab-read-word-at-point))))
          :desc "Run command" "r"     (lambda (arg)
                                        (interactive "P")
                                        (if arg
                                            (call-interactively
                                             #'dc-matlab-run-command)
                                          (call-interactively
                                           #'dc-matlab-run-last-command)))

          :desc "Close figures" "c"    #'dc-matlab-close-figures
          :desc "Apropos" "l"          #'matlab-shell-apropos
          :desc "Matlab shell" ","  (lambda (arg)
                                      (interactive "p")
                                      (cond ((= arg 4)
                                             (dc-matlab-shell-toggle))
                                            ((= arg 16)
                                             (dc-matlab-shell-here))
                                            (t (dc-matlab-shell))))
          (:prefix ("t" . "test")
            :desc "Toggle test file" "g"   #'dc-matlab-toggle-test-file
            :desc "Find test file" "G" (lambda! (counsel-find-file (dc-matlab-get-test-dir)))
            :desc "Run unit tests" "t"        (lambda! (dc-matlab-shell-run-tests
                                                  "file" "Unit"))
            :desc "Run all unit tests" "T"    (lambda! (dc-matlab-shell-run-tests
                                                  "project" "Unit"))
            :desc "Run integration tests" "i" (lambda! (dc-matlab-shell-run-tests
                                                  "project" "Integration"))
            :desc "Run functional tests" "f"  (lambda! (dc-matlab-shell-run-tests
                                                  "project" "Functional"))
            :desc "Run perftests" "p"      (lambda! (dc-matlab-shell-run-performance-tests
                                               "file"))
            :desc "Run all pefrtests" "P"  (lambda! (dc-matlab-shell-run-performance-tests
                                               "project"))
            :desc "Rerun failed tests" "r" (lambda! (dc-matlab-shell-run-tests "rerun"))
            :desc "Show test summary" "s"  #'dc-matlab-shell-test-summary))
        (:prefix "C-c"
          "C-c" #'matlab-shell-run-cell
          "C-l" #'matlab-shell-save-and-go
          "C-e" #'dc-matlab-shell-run-line
          "C-r" #'dc-matlab-shell-run-region))

  (add-hook! 'matlab-shell-mode-hook
    (map! :mode matlab-shell-mode
          :n "C-p" #'matlab-shell-previous-matching-input-from-input
          :n "C-n" #'matlab-shell-next-matching-input-from-input
          :ni "C-d" #'matlab-shell-exit
          :i "C-SPC" #'matlab-shell-tab)))

(after! matlab
  (defun dc-matlab-run-last-command ()
    "Run the last command sent to the matlab shell buffer."
    (interactive)
    (let ((curr-buffer (buffer-name)))
      (switch-to-buffer (concat "*" matlab-shell-buffer-name "*"))
      (dc-matlab-run-command (comint-previous-input-string 0))
      (switch-to-buffer curr-buffer)))

  (defvar dc--matlab-history nil)
  (defun dc-matlab-run-command (&optional command)
    "Send a command to the running matlab shell buffer."
    (interactive)
    (let ((curr-buffer (buffer-name)))

      (if (not dc--matlab-history)
          (progn
            (switch-to-buffer (concat "*" matlab-shell-buffer-name "*"))
            (setq dc--matlab-history (mapcar 'list (cddr comint-input-ring)))
            (switch-to-buffer curr-buffer)))

      (if (not command)
          (progn
            (setq command (completing-read "Command: "
                                           dc--matlab-history
                                           nil nil))))

      (switch-to-buffer (concat "*" matlab-shell-buffer-name "*"))
      (matlab-shell-send-string (concat command "\n"))
      (add-to-list 'dc--matlab-history command)
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
                                       (dc--matlab-get-filename))
      (dc--matlab-find-test-file)))

  (defun dc-matlab-get-test-dir ()
    (interactive)
    (concat (projectile-project-root) "tests/"))

  (defun dc--matlab-get-filename (&optional test)
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
    (let* ((test-dir (dc-matlab-get-test-dir))
           (test-filename (dc--matlab-get-filename 'test))
           (test-file (concat test-dir test-filename)))

      (if (not (file-exists-p test-dir))
          (mkdir test-dir))

      (message test-file)
      (if (not (file-exists-p test-file))
          (let ((buffer (get-buffer-create test-filename)))
            (with-current-buffer (switch-to-buffer buffer)
              (matlab-mode)
              (undo-tree-undo)
              (evil-insert-state)
              (dc--matlab-insert-test-snippet)
              (write-file test-file)))
        (find-file test-file))))

  (defun dc--matlab-insert-test-snippet ()
    "Add snippet for matlab function when opening a new .m file."
    (yas-expand-snippet (yas-lookup-snippet 'unittest 'matlab-mode)))

  (defun dc-matlab-shell-run-tests (scope tag)
    (interactive)
    (let ((test-suite "testSuite = matlab.unittest.TestSuite.from%s('%s', 'tag', '%s'); ")
          (command (concat "testResults = run(%s); "
                           "utils.summarizeTests(testResults)")))

      (cond ((string= scope "file")
             (dc-matlab-run-command
              (concat
               (format test-suite
                       "File"
                       (concat (dc-matlab-get-test-dir)
                               (dc--matlab-get-filename 'test))
                       tag)
               (format command "testSuite"))))

            ((string= scope "project")
             (dc-matlab-run-command
              (concat
               (format test-suite "Folder" (dc-matlab-get-test-dir) tag)
               (format command "testSuite"))))

            ((string= scope "rerun")
             (dc-matlab-run-command
              (concat "testSuite = testSuite([testResults.Failed]);"
                      (format command "testSuite")))))))

  (defun dc-matlab-shell-run-performance-tests (scope)
    (interactive)
    (let ((command (concat  "testResults = runperf('%s', 'tag', 'Performance'); "
                            "utils.summarizeTests(testResults)")))
      (cond ((string= scope "file")
             (dc-matlab-run-command
              (format command (concat
                               (dc-matlab-get-test-dir)
                               (dc--matlab-get-filename 'test)))))

            ((string= scope "project")
             (dc-matlab-run-command
              (format command (dc-matlab-get-test-dir)))))))

  (defun dc-matlab-shell-test-summary ()
    (interactive)
    (dc-matlab-run-command "utils.summarizeTests(testResults)"))

  (defun dc-matlab-shell-run-line (n)
    (interactive "p")
    (let ((beg (point-at-bol))
          (end (point-at-eol n)))
      (save-excursion
        (goto-char beg)
        (push-mark-command 0)
        (goto-char end)
        (matlab-shell-run-region-or-line))
      (pop-mark)
      (evil-force-normal-state)))

  (evil-define-operator dc-matlab-shell-run-region (beg end)
    "Wrapper around `evilnc comment operator' to always comments whole lines"
    :move-point nil
    :type line
    :repeat t
    :jump t
    (interactive "<r>")
    (save-excursion
      (goto-char beg)
      (push-mark-command 0)
      (goto-char end)
      (matlab-shell-run-region-or-line))
    (pop-mark)
    (evil-force-normal-state))

  (defun dc-matlab-shell-here ()
    "Wrapper around `matlab-shell' to add persp support"
    (interactive)
    (let* ((buffer (switch-to-buffer "*MATLAB*"))
           (win (get-buffer-window buffer))
           (dir default-directory))
      (select-window win)
      (when (bound-and-true-p evil-local-mode)
        (evil-change-to-initial-state))
      (goto-char (point-max))
      (with-current-buffer (pop-to-buffer buffer)
        (if (not (eq major-mode 'matlab-shell-mode))
            (matlab-shell)
          (cd dir)
          (persp-add-buffer buffer)
          (run-mode-hooks 'matlab-shell-mode-hook)))))

  (defun dc-matlab-shell ()
    "Wrapper around `matlab-shell' to open in pop-up"
    (interactive)
    (let* ((buffer (pop-to-buffer "*MATLAB*"))
           (win (get-buffer-window buffer))
           (dir default-directory))
      (select-window win)
      (when (bound-and-true-p evil-local-mode)
        (evil-change-to-initial-state))
      (goto-char (point-max))
      (with-current-buffer (pop-to-buffer buffer)
        (if (not (eq major-mode 'matlab-shell-mode))
            (matlab-shell)
          (cd dir)
          (persp-add-buffer buffer)
          (run-mode-hooks 'matlab-shell-mode-hook)))))

  (defun dc-matlab-shell-toggle ()
    "Toggle a persistent matlab-shell window.

Mostly copied from `+shell/toggle'"
    (interactive)
    (evil-window-split)
    (evil-window-down 1)
    (let* ((buffer (switch-to-buffer "*MATLAB*"))
           (win (get-buffer-window buffer))
           (dir default-directory))
      (select-window win)
      (when (bound-and-true-p evil-local-mode)
        (evil-change-to-initial-state))
      (goto-char (point-max))
      (with-current-buffer (pop-to-buffer buffer)
        (if (not (eq major-mode 'matlab-shell-mode))
            (matlab-shell)
          (cd dir)
          (persp-add-buffer buffer)
          (run-mode-hooks 'matlab-shell-mode-hook))))))
