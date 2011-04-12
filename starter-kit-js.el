;;; starter-kit-js.el --- Some helpful Javascript helpers
;;
;; Part of the Emacs Starter Kit

(autoload 'espresso-mode "espresso" "Start espresso-mode" t)
(add-to-list 'auto-mode-alist '("\\.js$" . espresso-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . espresso-mode))
(add-hook 'espresso-mode-hook 'moz-minor-mode)
;(add-hook 'espresso-mode-hook 'esk-paredit-nonlisp)
(add-hook 'espresso-mode-hook 'run-coding-hook)
(setq espresso-indent-level 4)

;; If you prefer js2-mode, use this instead:
;; (add-to-list 'auto-mode-alist '("\\.js$" . espresso-mode))

(eval-after-load 'espresso
  '(progn
     ;;(define-key espresso-mode-map "{" 'paredit-open-curly)
     ;;(define-key espresso-mode-map "}" 'paredit-close-curly-and-newline)
     ;; fixes problem with pretty function font-lock
     (define-key espresso-mode-map (kbd ",") 'self-insert-command)
     (font-lock-add-keywords
      'espresso-mode `(("\\(function *\\)("
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
         (list (concat dotfiles-dir "bin/jsl") (list "--nologo" local-file))))

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

     (add-hook 'espresso-mode-hook 'turn-on-flymake-jsl)
     )
  )

(provide 'starter-kit-js)
;;; starter-kit-js.el ends here
