;;; esk/yasnippet.el --- Yasnippet setup
;;

(require 'yasnippet)

(yas-global-mode 1)
(setq yas-prompt-functions '(yas/dropdown-prompt yas/ido-prompt yas/x-prompt))
(setq yas-wrap-around-region 'cua)
