;;; starter-kit-completion.el --- A few common completion tricks
(defun ac-eshell-mode-setup ()
  (add-to-list 'ac-sources 'ac-source-files-in-current-dir))

;; Live completion with auto-complete
;; (see http://cx4a.org/software/auto-complete/)
(require 'auto-complete-config nil t)

;; Do What I Mean mode
(setq ac-dwim t)
(ac-config-default)

;; set also the completion for eshell
(add-hook 'eshell-mode-hook 'ac-eshell-mode-setup)
;; custom keybindings to use tab, enter and up and down arrows
(define-key ac-complete-mode-map "\t" 'ac-expand)
(define-key ac-complete-mode-map "\r" 'ac-complete)
(define-key ac-complete-mode-map "\M-n" 'ac-next)
(define-key ac-complete-mode-map "\M-p" 'ac-previous)

;; Disabling Yasnippet completion

(eval-when-compile (require 'yasnippet))

(defun epy-snips-from-table (table)
  (with-no-warnings
    (let ((hashtab (ac-yasnippet-table-hash table))
          (parent (ac-yasnippet-table-parent table))
          candidates)
      (maphash (lambda (key value)
                 (push key candidates))
               hashtab)
      (identity candidates)
      )))

(defun epy-get-all-snips ()
  (let (candidates)
    (maphash
     (lambda (kk vv) (push (epy-snips-from-table vv) candidates)) yas--tables)
    (apply 'append candidates))
  )

(setq ac-ignores (concatenate 'list ac-ignores (epy-get-all-snips)))

(defun ac-python-mode-setup ()
  (add-to-list 'ac-sources 'ac-source-yasnippet))

(add-hook 'python-mode-hook 'ac-python-mode-setup)

(provide 'starter-kit-completion)
;;; starter-kit-completion.el ends here
