;;; esk/git.el --- Some git helpers
;;

(eval-after-load 'magit
  '(progn
     ;; full screen magit-status

     (defadvice magit-status (around magit-fullscreen activate)
       (window-configuration-to-register :magit-fullscreen)
       ad-do-it
       (delete-other-windows))

     (defadvice magit-mode-quit-window (around magit-restore-screen activate)
       ad-do-it
       (jump-to-register :magit-fullscreen))))

(require 'magit)

(defun esk/git-grep (command-args)
  "Use the `grep' machinery to run `git grep'.
Without a prefix argument the search is executed in the current Git repository,
otherwise it recurses down also in all submodules."
  (interactive
   (let ((what (or (thing-at-point 'symbol t)
                   (read-from-minibuffer "Regexp to search: ")))
         (git-grep "git --no-pager grep -n --color=always --full-name ")
         (git-submodule-foreach "git --no-pager submodule --quiet foreach ")
         (sed "sed s,^,$path/,")
         grep-command)
     (if current-prefix-arg
         (setq grep-command (concat git-grep "'" what "' && "
                                    git-submodule-foreach
                                    "'"
                                    git-grep "'" what "' | " sed " || :"
                                    "'"))
       (setq grep-command (concat git-grep "'" what "'")))
     (list (read-shell-command "Run: " grep-command))))
  (compilation-start (concat "cd " (magit-toplevel) " && " command-args) 'grep-mode))


(defun esk/parent-magit-status ()
  "Run magit-status on the parent repository"
  (interactive)
  (let ((parent-dir (or (locate-dominating-file
                         (file-name-directory (directory-file-name (magit-toplevel))) ".git") ".")))
    (if (equal parent-dir ".")
        (beep)
      (magit-status parent-dir))))
