;-*- coding: utf-8 -*-
;:Progetto:  dot.emacs -- Standard skeletons
;:Creato:    ven 06 feb 2004 00:56:45 CET
;:Autore:    Lele Gaifax <lele@metapensiero.it>
;:Licenza:   GNU General Public License version 3 or later
;

(require 'autoinsert)

(auto-insert-mode t)

(define-skeleton intestazione-gpl
  "Inserisci intestazione standard GPLv3."
  "[Progetto]: "
  comment-start " -*- coding: utf-8 -*- " comment-end "\n"
  comment-start ":Progetto:  " str " -- " _ comment-end "\n"
  comment-start ":Creato:    " (format-time-string "%c") comment-end "\n"
  comment-start ":Autore:    " (user-full-name) " <" user-mail-address ">" comment-end "\n"
  comment-start ":Licenza:   " "GNU General Public License version 3 or later" comment-end "\n"
  comment-start comment-end "\n\n")

(define-skeleton intestazione-airpim
  "Inserisci intestazione standard AirPIM."
  "[Progetto]: "
  comment-start " -*- coding: utf-8 -*- " comment-end "\n"
  comment-start ":Progetto:  " str " -- " _ comment-end "\n"
  comment-start ":Creato:    " (format-time-string "%c") comment-end "\n"
  comment-start ":Autore:    " (user-full-name) " <" user-mail-address ">" comment-end "\n"
  comment-start ":Licenza:   Copyright (C) " (format-time-string "%Y") " Airpim S.r.l. Tutti i diritti riservati." comment-end "\n"
  comment-start comment-end) "\n\n"

(define-skeleton intestazione-org
  "Intestazione per i file ORG."
  "[Titolo]:"
  "# -*- coding: utf-8 -*-\n"
  "#+TITLE: " str "\n"
  "#+AUTHOR: " (user-full-name) " <" user-mail-address "\n"
  "#+LANGUAGE: it\n"
  "#+CATEGORY: " _ "\n"
  "#+SEQ_TODO: TODO WONTDO WiP DONE\n"
  "#+PROPERTY: Effort_ALL 0 0:10 0:30 1:00 2:00 3:00 4:00 5:00 6:00 7:00 8:00\n"
  "#+COLUMNS: %40ITEM(Voce) %13Effort(Tempo stimato){:} %CLOCKSUM(Tempo effettivo)\n\n")

(define-skeleton intestazione-preventivo
  "Intestazione per i preventivi ORG."
  "[Titolo]:"
  "# -*- coding: utf-8 -*-\n"
  "#+TITLE: Preventivo " str "\n"
  "#+AUTHOR: " (user-full-name) " <" user-mail-address ">\n"
  "#+LANGUAGE: it\n"
  "#+CATEGORY: " _ "\n"
  "#+SEQ_TODO: TODO WONTDO WiP DONE\n"
  "#+PROPERTY: Effort_ALL 0 0:10 0:30 1:00 2:00 3:00 4:00 5:00 6:00 7:00 8:00\n"
  "#+COLUMNS: %40ITEM(Voce) %13Effort(Tempo stimato){:} %CLOCKSUM(Tempo effettivo)\n\n"
  "#+BEGIN: columnview :hlines 1 :id global\n"
  "#+END:\n\n"
  "* " str "\n\n"
  "** Preventivo\n\n"
  "*** Preanalisi\n"
  "    :PROPERTIES:\n"
  "    :Effort:   1:00\n"
  "    :END:\n\n"
  "*** Stima\n"
  "    :PROPERTIES:\n"
  "    :Effort:   0:30\n"
  "    :END:\n")

(define-skeleton intestazione-mako
  "Inserisci un blocco di commento con varie informazioni standard."
  "[Progetto]: "
  "## -*- coding: utf-8 -*-\n"
  "## :Progetto:  " str " -- " _ "\n"
  "## :Creato:    " (format-time-string "%c") "\n"
  "## :Autore:    " (user-full-name) " <" user-mail-address ">\n"
  "## :Licenza:   " "GNU General Public License version 3 or later\n"
  "##\n\n")

(setq auto-insert-alist
      (cons '(("\\.sql\\'" . "SQL header") . intestazione-gpl)
            auto-insert-alist))
(setq auto-insert-alist
      (cons '(("\\.rst\\'" . "ReST header") . intestazione-gpl)
            auto-insert-alist))
(setq auto-insert-alist
      (cons '(("\\.pt\\'" . "ZPT header") . intestazione-gpl)
            auto-insert-alist))
(setq auto-insert-alist
      (cons '(("\\.py\\'" . "Python header") . intestazione-gpl)
            auto-insert-alist))
(setq auto-insert-alist
      (cons '(("\\.js\\'" . "Javascript header") . intestazione-gpl)
            auto-insert-alist))
(setq auto-insert-alist
      (cons '(("\\.org\\'" . "ORG header") . intestazione-org)
            auto-insert-alist))

(provide 'starter-kit-skeletons)
;;; starter-kit-skeletons.el ends here
