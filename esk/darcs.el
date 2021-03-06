;;; esk/darcs.el --- Some darcs helpers
;;

;; darcsum -- http://joyful.com/repos/darcsum
(autoload 'darcsum-changes "darcsum" nil t)
(autoload 'darcsum-repository-root "darcsum" nil t)
(autoload 'darcsum-view "darcsum" nil t)
(autoload 'darcsum-whatsnew "darcsum" nil t)

;; vc-darcs
(add-to-list 'vc-handled-backends 'DARCS)

;; This is annoying: when recording a patch from the command line
;; the changelog becomes read-only...
;;(autoload 'vc-darcs-find-file-hook "vc-darcs")
;;(add-hook 'find-file-hooks 'vc-darcs-find-file-hook)
