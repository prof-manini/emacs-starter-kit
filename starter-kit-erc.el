;;; starter-kit-erc.el --- Some erc helpers
;;

(eval-after-load 'erc
  '(progn
     ;; logging
     (setq erc-log-insert-log-on-open nil)
     (setq erc-log-channels t)
     (setq erc-log-channels-directory "~/irclogs/")
     (setq erc-save-buffer-on-part t)
     (setq erc-hide-timestamps nil)

     (add-hook 'erc-insert-post-hook 'erc-save-buffer-in-logs)
     (add-hook 'erc-mode-hook '(lambda () (when (not (featurep 'xemacs))
                                       (set (make-variable-buffer-local
                                             'coding-system-for-write)
                                            'emacs-mule))))

     (setq erc-auto-set-away nil)
     (setq erc-autoaway-mode nil)
     (setq erc-modules (quote (autoaway autojoin button fill irccontrols
                                        match netsplit noncommands completion
                                        readonly ring smiley stamp track)))

     ;; paste2
     (autoload 'paste2-buffer-create "paste2" "create a buffer and then send its content to paste2.org." t)
     (define-key erc-mode-map (kbd "C-c p") 'paste2-buffer-create)))


(provide 'starter-kit-erc)
;;; starter-kit-erc.el ends here
