;;-*- coding: utf-8 -*-
;;:Progetto:  dot.emacs -- Lele's personal preferences
;;:Creato:    ven 09 apr 2010 01:08:16 CEST
;;:Autore:    Lele Gaifax <lele@metapensiero.it>
;;:Licenza:   GNU General Public License version 3 or later
;;

(csetq user-mail-address "lele@metapensiero.it")
(csetq user-full-name "Lele Gaifax")

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


;; projectile

(eval-after-load 'projectile
  '(progn
     ;; These are common associations in PatchDB context
     (add-to-list 'projectile-other-file-alist '("sql" "rst" "py"))
     (add-to-list 'projectile-other-file-alist '("rst" "sql" "py"))
     ;; And this is for javascripthon
     (add-to-list 'projectile-other-file-alist '("pj" "js" "py"))))


;; Automatically update copyright years when saving

(csetq copyright-names-regexp (regexp-quote (user-full-name)))
(csetq copyright-year-ranges t)
(add-hook 'before-save-hook #'copyright-update)


;; Monkey patch flymake-python-pyflakes-warn-regex function: ignore the argument and always
;; return the right regexp for pyflakes: I'm executing it with "python3.6 -m pyflakes" so the
;; original logic is wrong

(eval-after-load 'flymake-python-pyflakes
  '(defun flymake-python-pyflakes-warn-regex (executable)
     "Return a regex which identifies warnings output by EXECUTABLE."
     "\\(^redefinition\\|.*unused.*\\|used$\\)"))


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

  (if (or dont-ask (y-or-n-p "Emacs server? ")) (server-start))
  (if (or dont-ask (y-or-n-p "Notmuch? ")) (notmuch))
  (if (or dont-ask (y-or-n-p "GNUS? ")) (gnus))
  (if (or dont-ask (y-or-n-p "IRC? ")) (esk/start-erc-session))
  ;; (if (or dont-ask (y-or-n-p "Jabber? ")) (jabber-connect-all))

  (message "Have a nice day!"))

(defun esk/mine-emacs-! ()
  "Unconditionally start my emacs setup."
  (esk/mine-emacs t))


;; Use a nicer font

;(set-frame-font "DejaVu Sans Mono-10" t)
(set-frame-font "Cousine-10" t)


;; Enable some "dangerous" functionalities

(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)


;; Customize will write the settings here

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-revert-mode-text "")
 '(auto-revert-tail-mode-text "")
 '(canlock-password "4ed8bef8ca02417ad311454b547d2c0b6206cd99")
 '(fill-column 95)
 '(flymake-python-pyflakes-executable (quote "/usr/local/bin/python3.6"))
 '(flymake-python-pyflakes-extra-arguments (quote ("-m" "pyflakes")))
 '(git-commit-summary-max-length 70)
 '(ispell-dictionary "american")
 '(jedi:server-command (quote ("/usr/local/bin/jediepcserver")))
 '(longlines-show-hard-newlines t)
 '(magit-delete-by-moving-to-trash nil)
 '(message-fill-column 78)
 '(message-kill-buffer-on-exit t)
 '(notmuch-fcc-dirs "metapensiero/Sent")
 '(org-time-clocksum-format
   (quote
    (:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t)))
 '(package-selected-packages
   (quote
    (
     company
     company-jedi
     darcsum
     dockerfile-mode
     erc-hl-nicks
     expand-region
     flx-ido
     flymake-cursor
     flymake-python-pyflakes
     iedit
     js2-mode
     json-mode
     magit
     mo-git-blame
     nginx-mode
     notmuch
     notmuch-labeler
     projectile
     py-isort
     rainbow-mode
     smartparens
     vc-darcs
     web-mode
     wgrep
     whitespace-cleanup-mode
     yaml-mode
     yasnippet
     )))
 '(projectile-mode-line (quote (:eval (format " 〚%s〛" (projectile-project-name)))))
 '(python-fill-docstring-style (quote pep-257-nn))
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
