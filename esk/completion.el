;;; esk/completion.el --- A few common completion tricks
;;

(defun ac-eshell-mode-setup ()
  (add-to-list 'ac-sources 'ac-source-files-in-current-dir))

;; Live completion with auto-complete
;; (see http://cx4a.org/software/auto-complete/)
(require 'auto-complete-config nil t)

;; Do What I Mean mode
(setq ac-dwim t)
;; and don't try to be smarter than me on case match
(setq ac-ignore-case nil)
(ac-config-default)

;; set also the completion for eshell
(add-hook 'eshell-mode-hook 'ac-eshell-mode-setup)
;; custom keybindings to use tab, enter and up and down arrows
(define-key ac-complete-mode-map "\t" 'ac-expand)
(define-key ac-complete-mode-map "\r" 'ac-complete)
(define-key ac-complete-mode-map "\M-n" 'ac-next)
(define-key ac-complete-mode-map "\M-p" 'ac-previous)
