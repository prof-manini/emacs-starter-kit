;;; esk/elpa.el --- Install a base set of packages automatically.
;;

(require 'cl)

(defvar esk-packages
  (list
   'auto-complete
   'darcsum
   'erc-hl-nicks
   'find-file-in-project
   'flymake-cursor
   'flymake-python-pyflakes
;;; Use magit-next until it gets merged into master
;   'git-commit-mode
;   'git-rebase-mode
;   'magit
   'js2-mode
   'mo-git-blame
   'scss-mode
   'vc-darcs
   'whitespace-cleanup-mode
   'yasnippet
   )
  "Libraries that should be installed by default.")

(defun esk-install-packages ()
  "Install all starter-kit packages that aren't installed."
  (interactive)
  (dolist (package esk-packages)
    (unless (or (member package package-activated-list)
                (functionp package))
      (message "Installing %s" (symbol-name package))
      (package-install package))))

(defun esk-online? ()
  "See if we're online.

Windows does not have the network-interface-list function, so we
just have to assume it's online."
  ;; TODO how could this work on Windows?
  (if (and (functionp 'network-interface-list)
           (network-interface-list))
      (some (lambda (iface) (unless (equal "lo" (car iface))
                         (member 'up (first (last (network-interface-info
                                                   (car iface)))))))
            (network-interface-list))
    t))
