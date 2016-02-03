;;; esk/git.el --- Some git helpers
;;

(require 'magit)

(global-magit-file-mode)

(defun esk/git-outer-level (&optional dir)
  "Return the outer repository toplevel directory, or nil if there isn't one."
  (unless dir
    (setq dir (magit-toplevel)))
  (magit-toplevel (file-name-directory (directory-file-name dir))))

(defun esk/git-toplevel (&optional outmost)
  "Return the toplevel directory of current repository.
When OUTMOST is non-nil, assume this is a nested repository (a
submodule for example) and return the directory containing the
outmost repository instead."
  (let ((toplevel (magit-toplevel))
        outer)
    (when outmost
      (while (setq outer (esk/git-outer-level toplevel))
        (setq toplevel outer)))
    toplevel))

(defun esk/git-grep (command)
  "Use the `grep' machinery to run `git grep'.
Without a universal prefix argument, the search is executed only
in the current Git repository, otherwise it recurses down also in
all submodules.
With two univeral prefix arguments, the search starts in the
outmost Git repository recursing down in all submodules."
  (interactive
   (let ((what (or (thing-at-point 'symbol t)
                   (read-from-minibuffer "Regexp to search: ")))
         (toplevel (esk/git-toplevel (equal current-prefix-arg '(16))))
         (git-grep "git --no-pager grep -n --color=always ")
         grep-command)
     (if current-prefix-arg
         (let ((git-submodule-foreach "git --no-pager submodule --quiet foreach --recursive ")
               (sed (concat "sed \"s,^,$toplevel/$path/,;s,^"
                            (expand-file-name toplevel) ",,\"")))
           (setq grep-command (concat "(" git-grep "\"" what "\" || :) && "
                                      git-submodule-foreach
                                      "'(" git-grep "\"" what "\" | " sed ") || :'")))
       (setq grep-command (concat git-grep "\"" what "\"")))
     (list (read-shell-command "Run: " (concat "cd " toplevel " && " grep-command)))))
  (compilation-start command 'grep-mode))

(defun esk/parent-magit-status ()
  "Run magit-status on the parent repository"
  (interactive)
  (let ((parent-dir (esk/git-outer-level)))
    (if (not parent-dir)
        (beep)
      (magit-status-internal parent-dir))))
