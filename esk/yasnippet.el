;;; esk/yasnippet.el --- Yasnippet setup
;;

(require 'yasnippet)

(yas-global-mode 1)
(csetq yas-prompt-functions '(yas/dropdown-prompt yas/ido-prompt yas/x-prompt))
(csetq yas-wrap-around-region 'cua)
