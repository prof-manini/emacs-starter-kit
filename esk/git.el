;;; starter-kit-git.el --- Some git helpers
;;

(eval-after-load 'magit
  '(progn
     ;; (set-face-foreground 'magit-diff-add "green3")
     ;; (set-face-foreground 'magit-diff-del "red3")))

     ;; full screen magit-status

     (defadvice magit-status (around magit-fullscreen activate)
       (window-configuration-to-register :magit-fullscreen)
       ad-do-it
       (delete-other-windows))

     (defadvice magit-mode-quit-window (around magit-restore-screen activate)
       ad-do-it
       (jump-to-register :magit-fullscreen))))

(require 'magit)
