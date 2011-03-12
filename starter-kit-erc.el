;;; starter-kit-erc.el --- Some erc helpers
;;

(eval-after-load 'erc
  '(progn
     (when (require 'netrc nil t)
       (let ((freenode (netrc-machine (netrc-parse "~/.netrc") "freenode.net" t)))
         (setq freenode-password (netrc-get freenode "password")
               freenode-username (netrc-get freenode "login")
               erc-prompt-for-nickserv-password nil
               erc-nickserv-passwords '((freenode
                                         ((freenode-username . freenode-password)
                                        ; ("nick-two" . "password")
                                          ))))
         (add-hook 'erc-after-connect
                   '(lambda (SERVER NICK)
                      (erc-message
                       "PRIVMSG" (concat "NickServ identify " freenode-password)))))
       )

     ;; logging
     (setq erc-log-insert-log-on-open nil)
     (setq erc-log-channels t)
     (setq erc-log-channels-directory "~/irclogs/")
     (setq erc-save-buffer-on-part t)
     (setq erc-hide-timestamps nil)

     (add-hook 'erc-insert-post-hook 'erc-save-buffer-in-logs)
     (add-hook 'erc-mode-hook '(lambda () (when (not (featurep 'xemacs))
                                       (set (make-variable-buffer-local
                                             'coding-system-for-write)
                                            'emacs-mule))))

     (setq erc-auto-set-away nil)
     (setq erc-autoaway-mode nil)
     (setq erc-modules (quote (autoaway autojoin button fill irccontrols
                                        match netsplit noncommands completion
                                        readonly ring smiley stamp track)))))

(defun irc-maybe ()
  "Connect to IRC, but ask first."
  (interactive)
  
  (when (y-or-n-p "IRC? ")
    (progn
      (require 'erc)

      (erc-autojoin-mode 1)

      (set-frame-font "DejaVu Sans Mono-10")
      (erc-select :server "irc.freenode.net"
                  :port 6667
                  :nick erc-nick
                  :full-name (user-full-name))))
  
  (when (y-or-n-p "Emacs server? ")
    (progn
      (server-start))))


(provide 'starter-kit-erc)
;;; starter-kit-erc.el ends here
