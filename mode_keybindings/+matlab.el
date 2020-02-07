(after! matlab
  (map! :mode matlab-mode
        (:localleader
          :desc "Help" "?"            (lambda! (matlab-shell-describe-command
                                          (matlab-read-word-at-point)))
          :desc "Find function" "."   (lambda! (matlab-shell-locate-fcn
                                          (matlab-read-word-at-point)))
          :desc "Help prompt" "C-?"   #'matlab-shell-describe-command
          :desc "Find function prompt" "C-." #'matlab-shell-locate-fcn
          :desc "Run command" "R"      #'dc-matlab-run-command
          :desc "Run last command" "r" #'dc-matlab-run-last-command
          :desc "Close figures" "c"    #'dc-matlab-close-figures
          :desc "Apropos" "l"          #'matlab-shell-apropos
          :desc "Matlab Shell" "p"     #'matlab-shell
          (:prefix ("t" . "test")
            :desc "Toggle test file" "g"   #'dc-matlab-toggle-test-file
            :desc "Run tests" "t"          (lambda! (dc-matlab-shell-run-tests "file"))
            :desc "Run all tests" "T"      (lambda! (dc-matlab-shell-run-tests "project"))
            :desc "Run perftests" "p"      (lambda! (dc-matlab-shell-run-performance-tests
                                               "file"))
            :desc "Run all pefrtests" "P"  (lambda! (dc-matlab-shell-run-performance-tests
                                               "project"))
            :desc "Rerun failed tests" "r" (lambda! (dc-matlab-shell-run-tests "rerun"))
            :desc "Show test summary" "s"  #'dc-matlab-shell-test-summary))
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
                                       (dc--matlab-get-filename))
        (dc--matlab-find-test-file)))

  (defun dc--matlab-get-test-dir ()
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
    (let* ((test-dir (dc--matlab-get-test-dir))
           (test-filename (dc--matlab-get-filename 'test))
           (test-file (concat test-dir test-filename)))

      (if (not (file-exists-p test-dir))
          (mkdir test-dir))

      (message test-file)
      (if (not (file-exists-p test-file))
          (with-temp-buffer
            (dc--matlab-insert-test-snippet (substring test-filename 0 -2))
            (write-file test-file)
            (goto-char 0)))

      (find-file test-file)))

  (defun dc--matlab-insert-test-snippet (classname)
    "Add snippet for matlab function when opening a new .m file."
    (insert (concat "classdef " classname
                    " < matlab.unittest.TestCase & matlab.perftest.TestCase\n\n"
                    "\tproperties\n"
                    "\tend\n\n"
                    "\tmethods (TestMethodSetup)\n"
                    "\tend\n\n"
                    "\tmethods (Test, TestTags = {'Unit'})\n"
                    "\tend\n\n"
                    "\tmethods (Test, TestTags = {'Performance'})\n"
                    "\tend\n"
                    "end")))

  (defun dc-matlab-shell-run-tests (scope)
    (interactive)
    (let ((test-suite "testSuite = matlab.unittest.TestSuite.from%s('%s', 'tag', 'Unit'); ")
          (command (concat "testResults = run(%s); "
                           "utils.summarizeTests(testResults)")))

      (cond ((string= scope "file")
             (dc-matlab-run-command
              (concat
               (format test-suite
                       "File"
                       (concat (dc--matlab-get-test-dir)
                               (dc--matlab-get-filename 'test)))
               (format command "testSuite"))))

            ((string= scope "project")
             (dc-matlab-run-command
              (concat
               (format test-suite "Folder" (dc--matlab-get-test-dir))
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
                               (dc--matlab-get-test-dir)
                               (dc--matlab-get-filename 'test)))))

            ((string= scope "project")
             (dc-matlab-run-command
              (format command (dc--matlab-get-test-dir)))))))

  (defun dc-matlab-shell-test-summary ()
    (interactive)
    (dc-matlab-run-command "utils.summarizeTests(testResults)")))
