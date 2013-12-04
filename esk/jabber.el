;;; starter-kit-jabber.el --- Some jabber helpers
;;

(eval-after-load 'jabber
  '(progn
     (if (require 'notifications nil t)
         ;; emacs 24
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
       ;; emacs < 24
       (require 'notify)
       (defun jabber-notify (from buf text proposed-alert)
         "Notify of new Jabber chat messages via libnotify"
         (when (or jabber-message-alert-same-buffer
                   (not (memq (selected-window) (get-buffer-window-list buf))))
           (if (jabber-muc-sender-p from)
               (notify
                (format "(PM) %s"
                        (jabber-jid-displayname (jabber-jid-user from)))
                (format "%s: %s" (jabber-jid-resource from) text)
                :icon "/usr/share/notify-osd/icons/hicolor/scalable/status/notification-message-im.svg"
                :timeout 2000)
             (notify
              (format "%s" (jabber-jid-displayname from))
              text
              :icon "/usr/share/notify-osd/icons/hicolor/scalable/status/notification-message-im.svg"
              :timeout 2000)))))

     (add-hook 'jabber-alert-message-hooks 'jabber-notify)))
