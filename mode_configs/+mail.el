;;; ~/.doom.d/mode_configs/+mail.el -*- lexical-binding: t; -*-

(setq mu4e-maildir "~/.mail/gmail/")
(setq mail-user-agent 'message-user-agent)
(setq user-mail-address "davidconnell12@gmail.com"
      user-full-name "David R. Connell")

(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-stream-type 'ssl
      smtpmail-smtp-service 465)

(setq smtpmail-debug-info t)
(setq message-default-headers "Cc: \nBcc: \n")
(setq message-auto-save-directory "~/.mail/gmail/drafts")
(setq message-kill-buffer-on-exit t)
(setq message-directory "~/.mail/gmail")
