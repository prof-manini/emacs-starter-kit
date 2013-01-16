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

(setq gnus-read-newsrc-file nil)
(setq gnus-save-newsrc-file nil)

(setq gnus-permanently-visible-groups ".")
(setq gnus-summary-line-format "%U%R%z%d %(%[%-20,20a%]%) %I%s\n")

;; prefer plain text alternative
(setq mm-discouraged-alternatives '("text/html" "text/richtext" "image/.*")
      mm-automatic-display (remove "text/html" mm-automatic-display)
      gnus-buttonized-mime-types '("multipart/alternative" "multipart/signed"))

;; BBDB
;(require 'bbdb-gnus)
;(require 'bbdb-sc)
;(bbdb-insinuate-gnus)
;(bbdb-insinuate-sc)
;(bbdb-insinuate-message)

;;; per leggere l'email
;(setq gnus-select-method '(nnimap "arstecnica"
;                                 (nnimap-address "arstecnica.it")
;                                 (nnimap-nov-is-evil t)
;                                  (nnir-search-engine imap)
;                                 (nnimap-authinfo-file ".netrc")))

(setq gnus-select-method '(nntp "gmane"
                                (nntp-address "news.gmane.org")
                                (nnir-search-engine nntp)))

;;;per l'uso con supercite
(setq gnus-signature-separator
          "^\\(--  \\|^-- *\\|________\\========*\\)$")
(add-hook 'sc-pre-hook
          '(lambda ()
             (save-excursion
               (let ((start (point))
                     (end (mark t)))
                 (goto-char end)
                 (when (re-search-backward gnus-signature-separator start t)
                   (forward-line -1)
                   (while (looking-at "[ \t]*$")
                     (forward-line -1))
                   (forward-line 1)
                   (setq mark (set-marker (make-marker) (point)))
                   (delete-region mark (mark t)))))))

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

; Usa un frame separato per comporre l'email, lasciando intatto
; il frame originale.
;(gnus-add-configuration
; '(message
;       (frame 1.0
;             (if (not (buffer-live-p gnus-summary-buffer))
;                 (car (cdr (assoc 'group gnus-buffer-configuration)))
;		(car (cdr (assoc 'article gnus-buffer-configuration))))
;             (vertical ((user-position . t) (top . 1) (left . 1)
;                        (name . "Messaggio email"))
;			(message 1.0 point)))))
