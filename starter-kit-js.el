;;; starter-kit-js.el --- Some helpful Javascript helpers
;;
;; Part of the Emacs Starter Kit

(add-to-list 'auto-mode-alist '("\\.js$" . js-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))
(add-hook 'js-mode-hook 'moz-minor-mode)
;(add-hook 'js-mode-hook 'esk-paredit-nonlisp)
(add-hook 'js-mode-hook 'run-coding-hook)
(setq js-indent-level 4)

(eval-after-load 'js
  '(progn
     ;;(define-key js-mode-map "{" 'paredit-open-curly)
     ;;(define-key js-mode-map "}" 'paredit-close-curly-and-newline)
     ;; fixes problem with pretty function font-lock
     (define-key js-mode-map (kbd ",") 'self-insert-command)
     (font-lock-add-keywords
      'js-mode `(("\\(function *\\)("
                        (0 (progn (compose-region (match-beginning 1)
                                                  (match-end 1) "ƒ")
                                  nil)))))

     ;; Adapted from http://www.emacswiki.org/emacs/FlymakeJavaScript,
     ;; to http://www.javascriptlint.com/index.htm: we assume it has
     ;; been installed under ~/.emacs.d/, possibly using a virtualenv.

     (defun flymake-jsl-init ()
       (let* ((temp-file (flymake-init-create-temp-buffer-copy
                          'flymake-create-temp-inplace))
              (local-file (file-relative-name
                           temp-file
                           (file-name-directory buffer-file-name))))
         (list (concat esk-dotfiles-dir "bin/jsl") (list "--nologo" local-file))))

     (setq flymake-allowed-file-name-masks
           (cons '(".+\\.js$"
                   flymake-jsl-init
                   flymake-simple-cleanup
                   flymake-get-real-file-name)
                 flymake-allowed-file-name-masks))

     (setq flymake-err-line-patterns
           (cons '("^\\(.*\\)(\\([[:digit:]]+\\)): warning: \\(.+\\)$"
                   1 2 nil 3)
                 flymake-err-line-patterns))

     (defun turn-on-flymake-jsl ()
       (flymake-mode 1))

     (add-hook 'js-mode-hook 'turn-on-flymake-jsl)
     )
  )

(provide 'starter-kit-js)
;;; starter-kit-js.el ends here
