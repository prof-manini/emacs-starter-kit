;;; starter-kit-js.el --- Some helpful Javascript helpers
;;
;; Part of the Emacs Starter Kit

(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(eval-when-compile (require 'js2-mode))

(defun js2-apply-jsl-declares ()
  "Extract top level //jsl:declare XXX comments"
  (setq js2-additional-externs
        (nconc (js2-get-jsl-declares)
               js2-additional-externs)))

(defun js2-get-jsl-declares ()
  (loop for node in (js2-ast-root-comments js2-mode-ast)
        when (and (js2-comment-node-p node)
                  (save-excursion
                    (goto-char (+ 2 (js2-node-abs-pos node)))
                    (looking-at "jsl:declare ")))
        append (js2-get-jsl-declares-in
                (match-end 0)
                (js2-node-abs-end node))))

(defun js2-get-jsl-declares-in (beg end)
  (let (res)
    (save-excursion
      (goto-char beg)
      (while (re-search-forward js2-mode-identifier-re end t)
        (push (match-string-no-properties 0) res)))
    (nreverse res)))

(defun js2-electric-layout-post-self-insert-function ()
  "Like the standard, but indent the new line accordingly with the mode"
  (electric-layout-post-self-insert-function)
  (indent-according-to-mode))

(eval-after-load 'js2-mode
  '(progn
     (add-hook 'js2-mode-hook 'run-coding-hook)

     ;; Prettify the function keyword
     (font-lock-add-keywords
      'js2-mode `(("\\(function *\\)("
                   (0 (progn (compose-region (match-beginning 1)
                                             (match-end 1) "ƒ")
                             nil)))))

     ;; Register some mode-specific hooks
     (add-hook 'js2-mode-hook
               (lambda ()
                 (set-fill-column 95)
                 (add-hook 'post-self-insert-hook
                           'js2-electric-layout-post-self-insert-function
                           nil 'local)
                 (add-hook 'js2-post-parse-callbacks
                           'js2-apply-jsl-declares nil 'local)
                 (js2-reparse t)))

     ;; Nice warnings/errors summary on F7
     (define-key js2-mode-map [f7] 'js2-display-error-list)

     ;; Display warnings/errors on the mode line
     (add-hook 'js2-parse-finished-hook
               (lambda ()
                 (setq mode-name "JS2")
                 (when (> (length js2-parsed-errors) 0)
                   (setq mode-name
                         (concat mode-name
                                 (format ":%dE" (length js2-parsed-errors)))))
                 (when (> (length js2-parsed-warnings) 0)
                   (setq mode-name
                         (concat mode-name
                                 (format ":%dW" (length js2-parsed-warnings)))))
                 (force-mode-line-update)))))

(provide 'starter-kit-js)
;;; starter-kit-js.el ends here
