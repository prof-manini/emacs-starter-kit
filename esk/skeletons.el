;;; esk/skeletons.el --- Standard skeletons
;;

(require 'autoinsert)

(auto-insert-mode t)

(define-skeleton intestazione-gpl
  "Inserisci intestazione standard GPLv3."
  "[Progetto]: "
  comment-start `(delete-horizontal-space) " -*- coding: utf-8 -*-" comment-end "\n"
  comment-start `(delete-horizontal-space) " :Project:   " str " -- " _ comment-end "\n"
  comment-start `(delete-horizontal-space) " :Created:   " (format-time-string "%c") comment-end "\n"
  comment-start `(delete-horizontal-space) " :Author:    " (user-full-name) " <" user-mail-address ">" comment-end "\n"
  comment-start `(delete-horizontal-space) " :License:   GNU General Public License version 3 or later" comment-end "\n"
  comment-start `(delete-horizontal-space) " :Copyright: Copyright (C) " (format-time-string "%Y") " " (user-full-name) comment-end "\n"
  comment-start `(delete-horizontal-space) comment-end "\n\n")

(define-skeleton intestazione-gpl-block-comment
  "Inserisci intestazione standard GPLv3 (block comment)."
  "[Progetto]: "
  comment-start
  " -*- coding: utf-8 -*-\n"
  " * :Project:   " str " -- " _ "\n"
  " * :Created:   " (format-time-string "%c") "\n"
  " * :Author:    " (user-full-name) " <" user-mail-address ">" "\n"
  " * :License:   GNU General Public License version 3 or later\n"
  " * :Copyright: Copyright (C) " (format-time-string "%Y") " " (user-full-name) "\n"
  " " comment-end "\n\n")

(define-skeleton intestazione-org
  "Intestazione per i file ORG."
  "[Titolo]:"
  "# -*- coding: utf-8 -*-\n"
  "#+TITLE: " str "\n"
  "#+AUTHOR: " (user-full-name) " <" user-mail-address ">\n"
  "#+LANGUAGE: it\n"
  "#+CATEGORY: " _ "\n"
  "#+SEQ_TODO: TODO WONTDO WiP DONE\n"
  "#+PROPERTY: Effort_ALL 1:00 2:00 3:00 4:00 5:00 6:00 7:00 8:00\n"
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
  "## :Project:   " str " -- " _ "\n"
  "## :Created:   " (format-time-string "%c") "\n"
  "## :Author:    " (user-full-name) " <" user-mail-address ">\n"
  "## :License:   " "GNU General Public License version 3 or later\n"
  "## :Copyright: " "Copyright (C) " (format-time-string "%Y") " " (user-full-name) "\n"
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
      (cons '(("\\.css\\'" . "CSS header") . intestazione-gpl-block-comment)
            auto-insert-alist))
(setq auto-insert-alist
      (cons '(("\\.scss\\'" . "SCSS header") . intestazione-gpl-block-comment)
            auto-insert-alist))
(setq auto-insert-alist
      (cons '(("\\.org\\'" . "ORG header") . intestazione-org)
            auto-insert-alist))
