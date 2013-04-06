;;; starter-kit-erc.el --- Some erc helpers
;;

(defun erc-generate-log-file-name-brief (buffer target nick server port)
  "Computes a log file name from the TARGET and SERVER only.
This results in a filename of the form #channel@server.txt."
  (let ((file (concat target "@" server ".txt")))
    (convert-standard-filename file)))

(eval-after-load 'erc
  '(progn
     ;; logging
     (setq erc-log-insert-log-on-open nil)
     (setq erc-log-channels t)
     (setq erc-log-channels-directory "~/irclogs/")
     (setq erc-generate-log-file-name-function 'erc-generate-log-file-name-brief)
     (setq erc-save-buffer-on-part t)
     (setq erc-hide-timestamps nil)

     (add-hook 'erc-insert-post-hook 'erc-save-buffer-in-logs)
     (add-hook 'erc-mode-hook '(lambda ()
                                 (when (not (featurep 'xemacs))
                                   (set (make-variable-buffer-local
                                         'coding-system-for-write)
                                        'emacs-mule))))

     (setq erc-auto-set-away nil)
     (setq erc-autoaway-mode nil)
     (setq erc-modules (quote (autoaway autojoin button fill irccontrols
                                        match netsplit noncommands completion
                                        readonly ring smiley stamp track)))

     ;; notification
     (if (require 'notifications nil t)
         ;; emacs 24
         (defun erc-global-notify (match-type nick message)
           "Notify when a message is received."
           (notifications-notify
            :title (format "%s in %s"
                           (car (split-string nick "!"))
                           (or (erc-default-target) "#unknown"))
            :body (replace-regexp-in-string " +" " " message)
            :app-icon "/usr/share/notify-osd/icons/hicolor/scalable/status/notification-message-im.svg"
            :urgency 'low
            :timeout 2000))
       ;; emacs < 24
       (require 'notify)
       (defun erc-global-notify (match-type nick message)
         "Notify when a message is received."
         (notify
          (format "%s in %s"
                  (car (split-string nick "!"))
                  (or (erc-default-target) "#unknown"))
          (replace-regexp-in-string " +" " " message)
          :icon "/usr/share/notify-osd/icons/hicolor/scalable/status/notification-message-im.svg"
          :timeout 2000)))
     (add-hook 'erc-text-matched-hook 'erc-global-notify)))


(provide 'starter-kit-erc)
;;; starter-kit-erc.el ends here
