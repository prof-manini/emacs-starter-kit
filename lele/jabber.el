;;; lele/jabber.el --- Lele's JABBER specialization

(require 'netrc)

(eval-after-load 'jabber
  '(progn
     (setq jabber-account-list
           '(("lelegaifax@gmail.com"
              (:network-server . "talk.google.com")
              (:connection-type . ssl)))
           jabber-auto-reconnect t)

     ;; Extract password from ~/.netrc file and add it to the jabber-account-list. We extract
     ;; all the network-server entries from jabber-account-list, find the corresponding lines
     ;; in ~/.netrc, use the account name mentioned in there to find the entry in the
     ;; jabber-account-list, and add the password there. This means that the JID has to be
     ;; equal to the login value and the network-server has to be equal to the machine value.
     ;; This probably doesn't work for multiple JIDs connecting to the same network server,
     ;; yet.
     (dolist (server (mapcar (lambda (elem)
                               (cdr (assq :network-server (cdr elem))))
                             jabber-account-list))
       (let* ((data (netrc-machine (netrc-parse "~/.netrc") server t))
              (username (netrc-get data "login"))
              (password (netrc-get data "password"))
              (account (assoc username jabber-account-list)))
         (unless (assq :password (cdr account))
           (setcdr account (cons (cons :password password)
                                 (cdr account))))))))
