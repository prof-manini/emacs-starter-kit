;;; esk/lisp.el --- Some helpful Lisp code
;;

(define-key lisp-mode-shared-map (kbd "C-c l") "lambda")
(define-key lisp-mode-shared-map (kbd "C-c v") #'eval-buffer)

(defface esk-paren-face
   '((((class color) (background dark))
      (:foreground "grey50"))
     (((class color) (background light))
      (:foreground "grey55")))
   "Face used to dim parentheses."
   :group 'starter-kit-faces)

;;; Emacs Lisp

(defun pretty-lisp-lambdas ()
  (font-lock-add-keywords
   nil `(("(?\\(lambda\\>\\)"
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))

(add-hook 'emacs-lisp-mode-hook #'pretty-lisp-lambdas)
(add-hook 'emacs-lisp-mode-hook #'turn-on-eldoc-mode)
(add-hook 'emacs-lisp-mode-hook #'esk-remove-elc-on-save)

(defun esk-remove-elc-on-save ()
  "If you're saving an elisp file, likely the .elc is no longer valid."
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))))

(define-key emacs-lisp-mode-map (kbd "M-.") #'find-function-at-point)

;;; Clojure

(eval-after-load 'find-file-in-project
  '(add-to-list 'ffip-patterns "*.clj"))

(defun clojure-project ()
  (interactive)
  (message "Deprecated in favour of M-x swank-clojure-project. Install swank-clojure from ELPA."))

;;; Enhance Lisp Modes

(dolist (x '(scheme emacs-lisp lisp clojure))
  (when window-system
    (font-lock-add-keywords
     (intern (concat (symbol-name x) "-mode"))
     '(("(\\|)" . 'esk-paren-face))))
  (add-hook
   (intern (concat (symbol-name x) "-mode-hook")) #'run-coding-hook))

(eval-after-load 'clojure-mode
  '(font-lock-add-keywords
    'clojure-mode `(("(\\(fn\\>\\)"
                     (0 (progn (compose-region (match-beginning 1)
                                               (match-end 1) "ƒ")
                               nil))))))
