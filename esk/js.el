;;; starter-kit-js.el --- Some helpful Javascript helpers
;;

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
                 (add-hook 'js2-post-parse-callbacks
                           'js2-apply-jsl-declares nil 'local)
                 (js2-reparse t)))

     ;; Nice warnings/errors summary on F7
     (define-key js2-mode-map [f7] 'js2-display-error-list)

     ;; Bind js2-edit-extjs-template-at-point to C-c t
     (define-key js2-mode-map (kbd "C-c t") 'js2-edit-extjs-template-at-point)

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

;; Avoid escape nightmares by editing ExtJS templates in separate buffer
;;
;; Execute `M-x js2-edit-extjs-template-at-point' with the point inside the
;; following array of strings:
;;
;;   tpl: [
;;       '<div class=\'bh-summary\'>',
;;       '    <header>{header}</header>',
;;       '',
;;       '    <label>{fullname}</label>',
;;       '</div>'
;;   ]
;;
;; to edit the template in a separate buffer containing just the script, correctly handling
;; backslashed chars:
;;
;;   <div class='bh-summary'>
;;       <header>{header}</header>
;;
;;       <label>{fullname}</label>
;;   </div>
;;
;; Confirm the changes with `C-c C-c', abort them with `C-c C-k'.

(defun js2-edit-extjs-template-at-point ()
  (interactive)
  (let* ((ppss (syntax-ppss))
         (inner-sexp-start (nth 1 ppss))
         (current-line-offset (- (line-number-at-pos (point))
                                 (line-number-at-pos inner-sexp-start))))
    (when inner-sexp-start
      (save-excursion
        (goto-char inner-sexp-start)
        (when (looking-at "\\[[\s\n]+['\"]")
          (forward-sexp 1)
          (eet/edit (1+ inner-sexp-start) (1- (point)))
          (forward-line (1- current-line-offset)))))))

(defun eet/edit (start end)
  (let ((template (buffer-substring-no-properties start end))
        (original-buffer (current-buffer)))
    (select-window (split-window-vertically -4))
    (switch-to-buffer (generate-new-buffer "*template-edit*"))
    (insert template)
    (eet/implode)
    (set-buffer-modified-p nil)
    (goto-char (point-min))
    (when (looking-at "<")
      (html-mode))
    (enlarge-window (1- (line-number-at-pos (point-max))))
    (eet/mode 1)
    (set (make-local-variable 'eet/original-buffer) original-buffer)
    (set (make-local-variable 'eet/original-start) start)
    (set (make-local-variable 'eet/original-end) end)))

(defun eet/implode ()
  (whitespace-cleanup)
  (goto-char (point-min))
  (let ((start (point))
        quote)
    (while (re-search-forward "['\"]" (point-max) t)
      (setq quote (char-to-string (char-before)))
      (kill-region start (point))
      (end-of-line)
      (search-backward quote)
      (kill-line)
      (eet/unescape-line quote)
      (eet/unescape-line "\\")
      (forward-line)
      (setq start (point)))))

(defun eet/unescape-line (quote)
  (save-excursion
    (let (start)
      (beginning-of-line)
      (setq start (point))
      (end-of-line)
      (while (search-backward (concat "\\" quote) start t)
        (replace-match "")
        (insert quote)))))

(defun eet/abort ()
  "Used in template-edit-mode to close the popup window"
  (interactive)
  (kill-buffer)
  (delete-window))

(defun eet/conclude ()
  (interactive)
  (let ((line (eet/explode))
        (contents (buffer-substring-no-properties (point-min) (- (point-max) 2)))
        (original-buffer eet/original-buffer)
        (original-start eet/original-start)
        (original-end eet/original-end))
    (kill-buffer)
    (delete-window)
    (switch-to-buffer original-buffer)
    (goto-char original-start)
    (delete-char (- original-end original-start))
    (insert "\n")
    (insert contents)
    (insert "\n")
    (let ((end (1+ (point))))
      (goto-char original-start)
      (forward-line line)
      (indent-region original-start end))))

(defun eet/explode ()
  (whitespace-cleanup)
  (let ((current-line (line-number-at-pos (point)))
        start)
    (goto-char (point-min))
    (while (and (re-search-forward "^" nil t)
                (not (eobp)))
      (setq start (point))
      (eet/escape-line "\\")
      (eet/escape-line "'")
      (insert "'")
      (end-of-line)
      (insert "'")
      (unless (eobp)
        (insert ","))
      (forward-line)
      (setq start (point)))
    current-line))

(defun eet/escape-line (quote)
  (save-excursion
    (let (start)
      (beginning-of-line)
      (setq start (point))
      (end-of-line)
      (while (search-backward quote start t)
        (replace-match "")
        (insert "\\" quote)
        (backward-char 2)))))

(defvar eet/mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-k") 'eet/abort)
    (define-key map (kbd "C-c C-c") 'eet/conclude)
    map)
  "Keymap for template-edit minor mode.")

(define-minor-mode eet/mode
  "Minor mode for useful keybindings while editing template."
  nil " TemplateEdit" eet/mode-map)
