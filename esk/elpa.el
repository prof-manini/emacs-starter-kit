;;; esk/elpa.el --- Install a base set of packages automatically.
;;

(defvar esk/packages
  (list
   'auto-complete
   'darcsum
   'dash
   'erc-hl-nicks
   'expand-region
   'find-file-in-project
   'flymake-cursor
   'flymake-python-pyflakes
   'iedit
;;; Use magit-next until it gets merged into master
;   'git-commit-mode
;   'git-rebase-mode
;   'magit
   'js2-mode
   'json-mode
   'mo-git-blame
   'scss-mode
   'vc-darcs
   'wgrep
   'whitespace-cleanup-mode
   'yasnippet
   )
  "Libraries that should be installed by default.")

(defun esk/install-packages ()
  "Install all starter-kit packages that aren't installed."
  (interactive)
  (dolist (package esk/packages)
    (unless (or (member package package-activated-list)
                (functionp package))
      (message "Installing %s" (symbol-name package))
      (package-install package))))

(defun esk/upgrade-packages ()
  "Automatically upgrade installed packages to latest version."
  (package-list-packages)
  (package-menu-mark-upgrades)
  (let ((upgrades (package-menu--find-upgrades)))
    (if upgrades
        (package-menu-execute t))))

(defun esk/online? ()
  "See if we're online.

Windows does not have the network-interface-list function, so we
just have to assume it's online."
  ;; TODO how could this work on Windows?
  (if (functionp 'network-interface-list)
      (member 'up (mapcar (lambda (iface) (if (equal (car iface) "lo")
                                         nil
                                       (car (last (car (last (network-interface-info
                                                              (car iface))))))))
                          (network-interface-list)))
    t))
