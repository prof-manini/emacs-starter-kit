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
;; The template definition may contain a configuration object defining template member
;; functions, for example:
;;
;;   tpl: [
;;       '<div class=\'bh-summary\'>',
;;       '    <header>{header}</header>',
;;       '',
;;       '    <label>{[this.uppercase(fullname)]}</label>',
;;       '</div>',
;;       {
;;         uppercase: function(s) {
;;           return s.toUpperCase();
;;         }
;;       }
;;   ]
;;
;; Such object is ignored and left in place as-is.
;;
;; Confirm the changes with `C-c C-c', abort them with `C-c C-k'.

(defun js2-edit-extjs-template-at-point ()
  "Edit an array of strings representing an ExtJS template in a different buffer."
  (interactive)
  (let* ((ppss (syntax-ppss))
         (inner-sexp-start (nth 1 ppss))
         (current-line (line-number-at-pos (point))))
    (when inner-sexp-start
      (save-excursion
        (goto-char inner-sexp-start)
        (when (looking-at "\\[[\s\n]+['\"]")
          (eet/edit (1+ inner-sexp-start) (eet/find-template-end) current-line))))))

(defun eet/find-template-end ()
  "Find and return the end of the template."
  (let (end)
    (save-excursion
      (forward-sexp 1)
      (forward-char -1)
      (setq end (point))
      ;; Here the point is over the ending ']': go backward by one s-exp, that
      ;; may be either the last string, or the optional configuration object;
      ;; in the latter case, search backward for the comma separator
      (forward-sexp -1)
      (when (looking-at "{")
        (search-backward ",")
        (setq end (point))))
    end))

(defun eet/edit (start end current-line)
  "Extract the template from the array of strings into a new buffer."
  (let ((template (buffer-substring-no-properties start end))
        (original-buffer (current-buffer))
        (winconf (current-window-configuration)))
    (switch-to-buffer (generate-new-buffer "*template-edit*"))
    (delete-other-windows-internal)
    (insert template)
    (eet/implode)
    (set-buffer-modified-p nil)
    (goto-char (point-min))
    (when (looking-at "<")
      (html-mode))
    (eet/mode 1)
    (set (make-local-variable 'eet/original-buffer) original-buffer)
    (set (make-local-variable 'eet/original-start) start)
    (set (make-local-variable 'eet/original-end) end)
    (set (make-local-variable 'eet/original-line) current-line)
    (set (make-local-variable 'eet/original-winconf) winconf)
    (message "Type C-c C-c to confirm changes, C-c C-k to abort")))

(defun eet/implode ()
  "Remove string quotes from each line and unescape remaining text."
  (insert "\n")
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
  "Used in template-edit-mode to close the popup window."
  (interactive)
  (let ((winconf eet/original-winconf)
        (original-buffer eet/original-buffer)
        (original-line eet/original-line))
    (kill-buffer)
    (set-window-configuration winconf)
    (switch-to-buffer original-buffer)
    (goto-line original-line)))

(defun eet/conclude ()
  "Used in eet/mode to confirm changes and close the popup window."
  (interactive)
  (let ((line (eet/explode))
        (contents (buffer-substring-no-properties (point-min) (- (point-max) 2)))
        (original-buffer eet/original-buffer)
        (original-start eet/original-start)
        (original-end eet/original-end)
        (winconf eet/original-winconf))
    (kill-buffer)
    (set-window-configuration winconf)
    (switch-to-buffer original-buffer)
    (goto-char original-start)
    (delete-char (- original-end original-start))
    (insert "\n")
    (insert contents)
    ;; If we are looking at a comma it means that what follows is the template
    ;; configuration object, otherwise insert a newline to properly indent the
    ;; ending ']'
    (unless (looking-at ",")
      (insert "\n"))
    (let ((end (1+ (point))))
      (goto-char original-start)
      (forward-line line)
      (indent-region original-start end))))

(defun eet/explode ()
  "Reinsert string quotes around each line and separate them with an ending comma."
  (let ((current-line (line-number-at-pos (point)))
        start)
    (goto-char (point-max))
    (insert "\n")
    (whitespace-cleanup)
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
  "Keymap for eet/mode minor mode.")

(define-minor-mode eet/mode
  "Minor mode for useful keybindings while editing template."
  nil " TemplateEdit" eet/mode-map)
