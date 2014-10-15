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

(defun git-grep (command-args)
  "Use the `grep' machinery to run `git grep'.
Without a prefix argument the search is executed in the default-directory of the
current buffer, otherwise it considers the whole Git repository."
  (interactive
   (let ((sap (thing-at-point 'symbol))
         (grep-command "git grep -n --color=always "))
     (list (read-shell-command "Run git grep (like this): "
                               (if sap (concat grep-command sap)
                                 grep-command)))))
  (when current-prefix-arg
      (setq command-args (concat "cd " (magit-get-top-dir) " && " command-args)))
  ; pipe thru cat, to avoid the "terminal not fully functional" error
  (compilation-start (concat command-args " | cat") 'grep-mode))

(require 'magit)
