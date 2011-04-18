;; -*- Emacs-Lisp -*-
;;:Progetto:  dot.emacs -- Impostazioni principali, preferenze generali
;;:Creato il: Mon Feb  2 19:37:56 2004
;;

(put 'downcase-region 'disabled nil)

;;
;; Modes
;;

;; Adding hook to automatically open a rope project if there is one
;; in the current or in the upper level directory: this may be too
;; heavy to be the default
;; (add-hook 'python-mode-hook 'ropemacs-auto-open-project)

;; remove the rope hooks for now
(remove-hook 'python-mode-hook 'ac-python-mode-setup)
(remove-hook 'rope-open-project-hook 'ac-nropemacs-setup)
