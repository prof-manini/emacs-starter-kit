;;; esk/registers.el --- Set up registers
;;

;; Registers allow you to jump to a file or other location
;; quickly. Use C-x r j followed by the letter of the register (i for
;; init.el, r for this file) to jump to it.

;; You should add registers here for the files you edit most often.

(dolist (r `((?i (file . ,(concat esk/top-dir "init.el")))
             (?b (file . ,(concat esk/lisp-dir "bindings.el")))
             (?m (file . ,esk/user-specific-config))
             (?r (file . ,(concat esk/lisp-dir "registers.el")))))
  (set-register (car r) (cadr r)))
