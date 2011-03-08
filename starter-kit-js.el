;;; starter-kit-js.el --- Some helpful Javascript helpers
;;
;; Part of the Emacs Starter Kit

(autoload 'espresso-mode "espresso" "Start espresso-mode" t)
(add-to-list 'auto-mode-alist '("\\.js$" . espresso-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . espresso-mode))
(add-hook 'espresso-mode-hook 'moz-minor-mode)
;(add-hook 'espresso-mode-hook 'esk-paredit-nonlisp)
(add-hook 'espresso-mode-hook 'run-coding-hook)
(setq espresso-indent-level 2)

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
     ;; http://www.emacswiki.org/emacs/FlymakeJavaScript

     (defun flymake-jslint-init ()
       (let* ((temp-file (flymake-init-create-temp-buffer-copy
                          'flymake-create-temp-inplace))
              (local-file (file-relative-name
                           temp-file
                           (file-name-directory buffer-file-name))))
         (list "rhino" (list (concat dotfiles-dir "scripts/fulljslint.js") local-file))))

     (setq flymake-allowed-file-name-masks
           (cons '(".+\\.js$"
                   flymake-jslint-init
                   flymake-simple-cleanup
                   flymake-get-real-file-name)
                 flymake-allowed-file-name-masks))

     (setq flymake-err-line-patterns
           (cons '("^Lint at line \\([[:digit:]]+\\) character \\([[:digit:]]+\\): \\(.+\\)$"
                   nil 1 2 3)
                 flymake-err-line-patterns))

     (defun turn-on-flymake-jslint ()
       (flymake-mode 1))

     (add-hook 'espresso-mode-hook 'yas/minor-mode-on)
     (add-hook 'espresso-mode-hook 'turn-on-flymake-jslint)
     )
  )

(provide 'starter-kit-js)
;;; starter-kit-js.el ends here
