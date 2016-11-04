;;-*- coding: utf-8 -*-
;;:Progetto:  dot.emacs -- Lele's personal preferences
;;:Creato:    ven 09 apr 2010 01:08:16 CEST
;;:Autore:    Lele Gaifax <lele@metapensiero.it>
;;:Licenza:   GNU General Public License version 3 or later
;;

(csetq user-mail-address "lele@metapensiero.it")
(csetq user-full-name "Lele Gaifax")

;; magit: prevent warning about auto revert mode

(setq magit-last-seen-setup-instructions "1.4.0")


;; notmuch

(load (concat esk/user-specific-dir "notmuch"))


;; reStructuredText

(eval-when-compile (require 'rst))

(eval-after-load 'rst
  '(progn
     ;; automatically update contents summary
     (add-hook 'rst-adjust-hook #'rst-toc-update)

     ;; disable new auto indent
     (add-hook 'rst-mode-hook (lambda () (electric-indent-local-mode -1)))))

;; These are common associations in PatchDB context
(eval-after-load 'projectile
  '(progn
     (add-to-list 'projectile-other-file-alist '("sql" "rst" "py"))
     (add-to-list 'projectile-other-file-alist '("rst" "sql" "py"))))


;; Automatically update copyright years when saving

(csetq copyright-names-regexp (regexp-quote (user-full-name)))
(csetq copyright-year-ranges t)
(add-hook 'before-save-hook #'copyright-update)


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
  ;; (if (or dont-ask (y-or-n-p "Jabber? ")) (jabber-connect-all))

  (message "Have a nice day!"))

(defun esk/mine-emacs-! ()
  "Unconditionally start my emacs setup."
  (esk/mine-emacs t))


;; Enable some "dangerous" functionalities

(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)


;; Customize will write the settings here

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-revert-tail-mode-text "")
 '(canlock-password "4ed8bef8ca02417ad311454b547d2c0b6206cd99")
 '(fill-column 95)
 '(git-commit-summary-max-length 70)
 '(ispell-dictionary "american")
 '(jedi:server-command (quote ("/usr/local/bin/jediepcserver")))
 '(js2-include-jslint-globals nil)
 '(js2-indent-switch-body t)
 '(longlines-show-hard-newlines t)
 '(message-fill-column 78)
 '(message-kill-buffer-on-exit t)
 '(notmuch-fcc-dirs "metapensiero/Sent")
 '(package-selected-packages
   (quote
    (company-jedi company notmuch yasnippet yaml-mode whitespace-cleanup-mode wgrep web-mode vc-darcs sphinx-doc smartparens rainbow-mode notmuch-labeler nginx-mode mo-git-blame json-mode js2-mode jabber iedit idomenu google-contacts flymake-python-pyflakes flymake-cursor expand-region erc-hl-nicks emmet-mode dockerfile-mode darcsum cython-mode auto-complete)))
 '(python-fill-docstring-style (quote pep-257-nn))
 '(safe-local-variable-values
   (quote
    ((web-mode-engines-alist quote
                             (("jinja" . "\\.templ\\'")))
     (flymake-python-pyflakes-executable . "python3.5")
     (flymake-python-pyflakes-extra-arguments "-m" "pyflakes")
     (flymake-python-pyflakes-executable . "python3")
     (org-time-clocksum-format :days "%dd" :hours "%d" :require-minutes nil)
     (org-time-clocksum-use-effort-durations . t)
     (lexical-binding . t))))
 '(scss-compile-at-save nil)
 '(send-mail-function (quote smtpmail-send-it))
 '(smtpmail-smtp-server "mail.arstecnica.it")
 '(smtpmail-smtp-service 25)
 '(web-mode-markup-indent-offset 2)
 '(whitespace-line-column nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
