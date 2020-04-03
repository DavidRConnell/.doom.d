;;; ~/.doom.d/mode_configs/+lisp.el -*- lexical-binding: t; -*-

(defun stumpwm-connect ()
  "Start slime and connect to the lisp image that is running the
  swank server.  Must have \"(require 'swank)
  (swank:create-server)\" in your .stumpwmrc "
  (interactive)
  (sly-connect "127.0.0.1"  4004)
  (with-current-buffer (current-buffer)
    (rename-buffer "*sbcl-stumpwm-repl*")
    (sly-eval '(in-package :stumpwm))))

(defun stumpwm-disconnect ()
  "Disconnects from the swank server currently open."
  (interactive)
  (with-current-buffer
      (switch-to-buffer "*sbcl-stumpwm-repl*")
    (sly-disconnect)))

(after! (:and lisp-mode evil-surrond)
  (add-to-list 'evil-surround-pairs-alist
               '(?* . ("*" . "*"))))
;; (add-hook 'lisp-mode-hook
;;           'stumpwm-connect)
