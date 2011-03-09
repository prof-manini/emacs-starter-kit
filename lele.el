;-*- coding: utf-8 -*-
;:Progetto:  dot.emacs -- Lele's personal preferences
;:Creato:    ven 09 apr 2010 01:08:16 CEST
;:Autore:    Lele Gaifax <lele@metapensiero.it>
;:Licenza:   GNU General Public License version 3 or later
;

(setq user-mail-address "lele@metapensiero.it")


;; Claws-mail

(setq auto-mode-alist
      (cons '("/tmpmsg\\." . text-mode)
            auto-mode-alist))
(setq magic-mode-alist nil)


;; ERC

(setq erc-autojoin-channels-alist
      '(("freenode.net"
         "#rafanass" "#linuxtrent" "#darcs" "#revctrl"
         "#tailor" "#sqlalchemy" "#airpim")))
(setq erc-email-userid user-mail-address)
(setq erc-nick "lelit")


;; GNUS

(setq gnus-home-directory (concat user-specific-dir "/gnus/"))
(setq gnus-read-newsrc-file nil)
(setq gnus-save-newsrc-file nil)

(setq gnus-default-posting-charset (quote utf-8))
(setq gnus-permanently-visible-groups ".")
(setq gnus-summary-line-format "%U%R%z%d %(%[%-20,20a%]%) %I%s\n")
