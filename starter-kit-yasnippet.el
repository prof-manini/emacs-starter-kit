; -*- coding: utf-8 -*-
;:Progetto:  dot.emacs -- Yasnippet setup
;:Creato:    lun 31 dic 2012 16:41:54 CET
;:Autore:    Lele Gaifax <lele@metapensiero.it>
;:Licenza:   GNU General Public License version 3 or later
;

(require 'yasnippet)

(yas-global-mode 1)
(setq yas-prompt-functions '(yas/dropdown-prompt yas/ido-prompt yas/x-prompt))
(setq yas-wrap-around-region 'cua)

(provide 'starter-kit-yasnippet)
;;; starter-kit-yasnippet.el ends here
