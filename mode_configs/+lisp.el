;;; ~/.doom.d/mode_configs/+lisp.el -*- lexical-binding: t; -*-
(add-hook! 'lisp-mode-hook
  '((if (is-in-stumpwm-dir (buffer-file-name (current-buffer)))
      (start-stumpwm-mrepl))))

(defun is-in-stumpwm-dir (buffer)
  (string-match-p (regexp-quote "stumpwm") buffer))

(defun start-stumpwm-mrepl ()
  (if (not (get-stumpwm-repl-buffer))
      (stumpwm-connect))
  (+popup-buffer (get-stumpwm-repl-buffer)))

(defun get-stumpwm-repl-buffer ()
  (find-if
   #'(lambda (buffer) (string-match-p (regexp-quote "sly-mrepl for sbcl")
                            (buffer-name buffer)))
   (buffer-list)))

(defun stumpwm-connect ()
  (sly-connect "127.0.0.1" "4004"))

(defun open-stumpwm-repl ()
  (let ((stumpwm-buffer (get-stumpwm-repl-buffer)))
    (if (not stumpwm-buffer)
        (stumpwm-connect)
        (delete-other-windows))))
  (switch-to-buffer (get-stumpwm-repl-buffer))
