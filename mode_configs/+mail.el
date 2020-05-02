;;; ~/.doom.d/mode_configs/+mail.el -*- lexical-binding: t; -*-

;; (setq mu4e-maildir "~/.mail/")
;; (setq mail-user-agent 'message-user-agent)

;; (setq message-send-mail-function 'smtpmail-send-it
;;       smtpmail-smtp-server "smtp.gmail.com"
;;       smtpmail-stream-type 'ssl
;;       smtpmail-smtp-service 465)

;; (setq smtpmail-debug-info t)
;; (setq message-auto-save-directory "~/News/drafts")
;; (setq message-kill-buffer-on-exit t)
;; (setq message-directory "~/News/")

;; (setq shr-color-visible-luminance-min 80)
;; (setq message-signature
;;       "Thank you\nDavid R. Connell")

;; (add-hook! 'mu4e-compose-mode-hook
;;   (turn-off-auto-fill)
;;   (visual-line-mode))

(setq message-default-headers "Cc: \nBcc: \n")
(set-email-account! "gmail"
                    '((user-full-name         . "David R. Connell")
                      (user-mail-address      . "davidconnell12@gmail.com")
                      (smtpmail-smtp-user     . "davidconnell12@gmail.com")
                      (mu4e-sent-folder       . "/gmail/Sent")
                      (mu4e-drafts-folder     . "/gmail/Drafts")
                      (mu4e-trash-folder      . "/gmail/Trash")
                      (mu4e-refile-folder     . "/gmail/All Mail")
                      (mu4e-compose-signature . "Thank you\nDavid R. Connell"))
                    t)

(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials '(("smtp.gmail.com" 587 "davidconnell12@gmail.com" nil))
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-local-domain "gmail.com")
