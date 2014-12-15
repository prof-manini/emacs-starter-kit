;;; esk/jabber.el --- Some jabber helpers
;;

(eval-when-compile
  (require 'jabber-alert)
  (require 'jabber-muc)
  (require 'jabber-util)
  (require 'notifications))

(eval-after-load 'jabber
  '(progn
     (defun jabber-notify (from buf text proposed-alert)
       "Notify of new Jabber chat messages"
       (when (or jabber-message-alert-same-buffer
                 (not (memq (selected-window) (get-buffer-window-list buf))))
         (if (jabber-muc-sender-p from)
             (notifications-notify
              :title (format "(PM) %s"
                             (jabber-jid-displayname (jabber-jid-user from)))
              :body (format "%s: %s" (jabber-jid-resource from) text)
              :app-icon "/usr/share/notify-osd/icons/hicolor/scalable/status/notification-message-im.svg"
              :urgency 'low
              :timeout 2000)
           (notifications-notify
            :title (format "%s" (jabber-jid-displayname from))
            :body text
            :app-icon "/usr/share/notify-osd/icons/hicolor/scalable/status/notification-message-im.svg"
            :urgency 'low
            :timeout 2000))))

     (add-hook 'jabber-alert-message-hooks 'jabber-notify)))
