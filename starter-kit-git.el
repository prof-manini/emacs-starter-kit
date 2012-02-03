;;; starter-kit-git.el --- Some git helpers
;;

;; egg -- https://github.com/byplayer/egg
(require 'egg)

;; Switch to the status buffer, instead of splitting current one
(setq egg-switch-to-buffer t)

;; (eval-after-load 'magit
;;   '(progn
;;      (set-face-foreground 'magit-diff-add "green3")
;;      (set-face-foreground 'magit-diff-del "red3")))

(provide 'starter-kit-git)
;;; starter-kit-git.el ends here
