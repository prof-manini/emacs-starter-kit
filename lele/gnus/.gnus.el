;-*- coding: utf-8 -*-
;:Progetto: dot.emacs -- configurazione di Gnus
;:Creato:   sab 07 feb 2004 03:56:23 CET
;:Autore:   Lele Gaifax <lele@nautilus.homeip.net>
;:Licenza:  GNU General Public License version 3 or later
;

(eval-when-compile
  (require 'gnus)
  (require 'gnus-start)
  (require 'gnus-art)
  (require 'gnus-msg)
  (require 'mm-decode))

;; Since I only use Gnus to read the newsgroups, disable reading and writing
;; the generic newsrc file
(setq gnus-read-newsrc-file nil)
(setq gnus-save-newsrc-file nil)

;; Also, I manually select which newsgroups I'm gonna follow, so don't bother
;; with keeping a notion of "new" newsgroups
(setq gnus-save-killed-list nil)
(setq gnus-check-new-newsgroups nil)

(setq gnus-permanently-visible-groups ".")
(setq gnus-summary-line-format "%U%R%z%d %(%[%-20,20a%]%) %I%s\n")
(setq gnus-summary-same-subject "⤷")

;; Prefer plain text alternative
(setq mm-discouraged-alternatives '("text/html" "text/richtext" "image/.*")
      mm-automatic-display (remove "text/html" mm-automatic-display)
      gnus-buttonized-mime-types '("multipart/alternative" "multipart/signed"))

(setq gnus-select-method '(nntp "gmane"
                                (nntp-address "news.gmane.org")
                                (nnir-search-engine nntp)))

;; Archive - putting sent mail and news somewhere
(setq gnus-message-archive-group
      '((if (message-news-p)
            (concat "sent-news-"
                    (format-time-string "%b-%y"))
          (concat "sent-mail-"
                  (format-time-string "%b-%y")))))

(setq gnus-posting-styles
      '((".*"
         (signature-file "~/.signature")
         (name "Lele Gaifax")
         (organization "Nautilus Entertainments"))
        ("^mail.+:"
         (name "Lele Gaifax")
         (organization "Nautilus Entertainments")
         (signature-file "~/.mail-signature"))))
