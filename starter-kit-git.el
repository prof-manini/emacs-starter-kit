;;; starter-kit-git.el --- Some git helpers
;;

;; egg -- https://github.com/byplayer/egg
(require 'egg)

;; Switch to the status buffer, instead of splitting current one
(setq egg-switch-to-buffer t)

;; Map nicer git-specific alternative of vc-annotate on S-F9
(autoload 'mo-git-blame-current "mo-git-blame" nil t)
(define-key egg-minor-mode-map [S-f9] 'mo-git-blame-current)

;; (eval-after-load 'magit
;;   '(progn
;;      (set-face-foreground 'magit-diff-add "green3")
;;      (set-face-foreground 'magit-diff-del "red3")))

(provide 'starter-kit-git)
;;; starter-kit-git.el ends here
