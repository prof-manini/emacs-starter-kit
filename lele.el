;;-*- coding: utf-8 -*-
;;:Progetto:  dot.emacs -- Lele's personal preferences
;;:Creato:    ven 09 apr 2010 01:08:16 CEST
;;:Autore:    Lele Gaifax <lele@metapensiero.it>
;;:Licenza:   GNU General Public License version 3 or later
;;

(setq user-mail-address "lele@metapensiero.it"
      user-full-name "Lele Gaifax")


;; reStructuredText

(eval-when-compile (require 'rst))

(eval-after-load 'rst
  '(progn
     ;; automatically update contents summary
     (add-hook 'rst-adjust-hook #'rst-toc-update)

     ;; disable new auto indent
     (add-hook 'rst-mode-hook (lambda () (electric-indent-local-mode -1)))))


;; Customize my main Emacs instance: I'm used to have one Emacs dedicated to
;; news, mail, chat and so on, living in the second i3 workspace. This function
;; is then called by my i3 configuration file with
;;
;;  exec --no-startup-id i3-msg 'workspace 2; exec emacs -f mine-emacs-!; workspace 1'

(defun esk/mine-emacs (&optional dont-ask)
  "Connect to IRC, GNUS, Jabber, Notmuch and activate Emacs server, but ask first.
If DONT-ASK is non-nil, interactively when invoked with a prefix arg,
start everything unconditionally."
  (interactive "P")

  (set-frame-font "DejaVu Sans Mono-10" t)

  (if (or dont-ask (y-or-n-p "Emacs server? ")) (server-start))
  (if (or dont-ask (y-or-n-p "Notmuch? ")) (notmuch))
  (if (or dont-ask (y-or-n-p "GNUS? ")) (gnus))
  (if (or dont-ask (y-or-n-p "IRC? ")) (esk/start-erc-session))
  (if (or dont-ask (y-or-n-p "Jabber? ")) (jabber-connect-all))

  (message "Have a nice day!"))

(defun esk/mine-emacs-! ()
  "Unconditionally start my emacs setup."
  (esk/mine-emacs t))


;; Customize will write the settings here

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(canlock-password "4ed8bef8ca02417ad311454b547d2c0b6206cd99")
 '(fill-column 95)
 '(git-commit-summary-max-length 70)
 '(js2-include-jslint-globals nil)
 '(js2-indent-switch-body t)
 '(longlines-show-hard-newlines t)
 '(message-fill-column 78)
 '(message-kill-buffer-on-exit t)
 '(notmuch-fcc-dirs "metapensiero/Sent")
 '(scss-compile-at-save nil)
 '(send-mail-function (quote smtpmail-send-it))
 '(smtpmail-smtp-server "mail.arstecnica.it")
 '(smtpmail-smtp-service 25)
 '(whitespace-action (quote (auto-cleanup)))
 '(whitespace-line-column nil)
 '(windmove-window-distance-delta 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
