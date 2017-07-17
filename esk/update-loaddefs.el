;;; esk/update-loaddefs.el --- Utility function to create loaddefs.el for current directory
;;

(fset 'message (lambda (&rest _)))
(setq make-backup-files nil)
(setq vc-handled-backends nil)
(setq default-directory (file-truename default-directory))
(setq generated-autoload-file (expand-file-name "loaddefs.el"))
(setq find-file-visit-truename t)
(update-directory-autoloads default-directory)
