;;; starter-kit-darcs.el --- Some darcs helpers
;;

;; darcsum -- http://joyful.com/repos/darcsum
(autoload 'darcsum-changes "darcsum" nil t)
(autoload 'darcsum-whatsnew "darcsum" nil t)
(autoload 'darcsum-view "darcsum" nil t)

;; vc-darcs
(add-to-list 'vc-handled-backends 'DARCS)
(autoload 'vc-darcs-find-file-hook "vc-darcs")
(add-hook 'find-file-hooks 'vc-darcs-find-file-hook)

(provide 'starter-kit-darcs)
;;; starter-kit-darcs.el ends here
