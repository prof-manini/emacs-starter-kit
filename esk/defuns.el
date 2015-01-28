;;; esk/defuns.el --- Define some custom functions
;;

(eval-when-compile
  (require 'desktop)
  (require 'virtualenv))

(require 'imenu)
(require 'thingatpt)
(require 'whitespace)
(require 'whitespace-cleanup-mode)

;; Network

(defun esk/view-url ()
  "Open a new buffer containing the contents of URL."
  (interactive)
  (let* ((default (thing-at-point-url-at-point))
         (url (read-from-minibuffer "URL: " default)))
    (switch-to-buffer (url-retrieve-synchronously url))
    (rename-buffer url t)
    ;; TODO: switch to nxml/nxhtml mode
    (cond ((search-forward "<?xml" nil t) (xml-mode))
          ((search-forward "<html" nil t) (html-mode)))))

;; Buffer-related

(defun esk/ido-imenu ()
  "Update the imenu index and then use ido to select a symbol to navigate to.
Symbols matching the text at point are put first in the completion list."
  (interactive)
  (imenu--make-index-alist)
  (let ((name-and-pos '())
        (symbol-names '()))
    (cl-labels ((addsymbols (symbol-list)
                            (when (listp symbol-list)
                              (dolist (symbol symbol-list)
                                (let ((name nil) (position nil))
                                  (cond
                                   ((and (listp symbol) (imenu--subalist-p symbol))
                                    (addsymbols symbol))

                                   ((listp symbol)
                                    (setq name (car symbol))
                                    (setq position (cdr symbol)))

                                   ((stringp symbol)
                                    (setq name symbol)
                                    (setq position (get-text-property 1 'org-imenu-marker symbol))))

                                  (unless (or (null position) (null name))
                                    (add-to-list 'symbol-names name)
                                    (add-to-list 'name-and-pos (cons name position))))))))
      (addsymbols imenu--index-alist))
    ;; If there are matching symbols at point, put them at the beginning of `symbol-names'.
    (let ((symbol-at-point (thing-at-point 'symbol)))
      (when symbol-at-point
        (let* ((regexp (concat (regexp-quote symbol-at-point) "$"))
               (matching-symbols (delq nil (mapcar (lambda (symbol)
                                                     (if (string-match regexp symbol) symbol))
                                                   symbol-names))))
          (when matching-symbols
            (sort matching-symbols (lambda (a b) (> (length a) (length b))))
            (mapc (lambda (symbol) (setq symbol-names (cons symbol (delete symbol symbol-names))))
                  matching-symbols)))))
    (let* ((selected-symbol (ido-completing-read "Symbol? " symbol-names))
           (position (cdr (assoc selected-symbol name-and-pos))))
      (goto-char position))))

;;; These belong in esk/coding-hook:

;; We have a number of turn-on-* functions since it's advised that lambda
;; functions not go in hooks. Repeatedly evaling an add-to-list with a
;; hook value will repeatedly add it since there's no way to ensure
;; that a lambda doesn't already exist in the list.

(defun esk/local-column-number-mode ()
  (make-local-variable 'column-number-mode)
  (column-number-mode t))

(defun esk/local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode t))

(defun esk/turn-on-hl-line-mode ()
  (if window-system (hl-line-mode t)))

(defun esk/turn-off-tool-bar ()
  (tool-bar-mode -1))

(defun esk/add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\|TODO\\|FIXME\\|HACK\\|REFACTOR\\|XXX\\):"
          1 font-lock-warning-face t))))

(defun esk/remove-whitespace-write-file-hook ()
  (remove-hook 'write-file-functions #'whitespace-write-file-hook t))

(add-hook 'whitespace-mode-hook #'esk/remove-whitespace-write-file-hook)

(defun esk/turn-on-whitespace-mode-makefiles ()
  (setq indent-tabs-mode t)
  (add-to-list (make-local-variable 'whitespace-style) 'indentation::tab)
  (whitespace-turn-on))

(defun esk/turn-on-subword-mode ()
  (subword-mode 1)
  ; change the minor mode lighter from the default " ," to something
  ; better
  (let ((entry (assq 'subword-mode minor-mode-alist)))
    (when entry (setcdr entry '(" sw")))))

(defun esk/set-string-delimiters-electric-pairs ()
  (set (make-local-variable 'electric-pair-pairs) '((?\" . ?\") (?\' . ?\'))))

(add-hook 'esk/coding-hook #'esk/local-column-number-mode)
(add-hook 'esk/coding-hook #'esk/local-comment-auto-fill)
(add-hook 'esk/coding-hook #'esk/turn-on-hl-line-mode)
(add-hook 'esk/coding-hook #'esk/add-watchwords)
(add-hook 'esk/coding-hook #'turn-on-whitespace-cleanup-mode)
(add-hook 'esk/coding-hook #'whitespace-turn-on)
(add-hook 'esk/coding-hook #'esk/turn-on-subword-mode)
(add-hook 'esk/coding-hook #'esk/set-string-delimiters-electric-pairs)

(defun esk/run-coding-hook ()
  "Enable things that are convenient across all coding buffers."
  (run-hooks 'esk/coding-hook))

(defun esk/untabify-buffer ()
  "Replace TABs with spaces in the whole buffer."
  (interactive)
  (untabify (point-min) (point-max)))

(defun esk/indent-buffer ()
  "Reindent the whole buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

;; Other

(defun esk/eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun esk/recompile-init ()
  "Byte-compile all your dotfiles again."
  (interactive)
  (byte-recompile-directory esk/top-dir 0)
  ;; TODO: remove elpa-to-submit once everything's submitted.
  (byte-recompile-directory (concat esk/top-dir "elpa-to-submit/") 0))

(defun esk/sudo-edit (&optional arg)
  "Edit a file as root."
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun esk/lorem ()
  "Insert a lorem ipsum."
  (interactive)
  (insert "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do "
          "eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim"
          "ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut "
          "aliquip ex ea commodo consequat. Duis aute irure dolor in "
          "reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla "
          "pariatur. Excepteur sint occaecat cupidatat non proident, sunt in "
          "culpa qui officia deserunt mollit anim id est laborum."))

(defun esk/switch-or-start (function buffer)
  "If the buffer is current, bury it, otherwise invoke the function."
  (if (equal (buffer-name (current-buffer)) buffer)
      (bury-buffer)
    (if (get-buffer buffer)
        (switch-to-buffer buffer)
      (funcall function))))

(defun esk/insert-date ()
  "Insert a time-stamp according to locale's date and time format."
  (interactive)
  (insert (format-time-string "%c" (current-time))))

(defun esk/pairing-bot ()
  "If you can't pair program with a human, use this instead."
  (interactive)
  (message (if (y-or-n-p "Do you have a test for that? ") "Good." "Bad!")))

(defun esk/toggle-fullscreen ()
  (interactive)
  ;; TODO: this only works for X. patches welcome for other OSes.
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                         '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                         '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0)))

(defun esk/toggle-window-split ()
  "Toggle between horizontal and vertical layout of two windows."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  #'split-window-horizontally
                #'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(defun esk/whatsnew-or-magit-status-or-vc-dir ()
  "Run either darcsum-whatsnew or vc-dir accordingly with current vc-backend"
  (interactive)
  (eval-when-compile
    (require 'darcsum)
    (require 'magit))
  (let ((backend (vc-backend (buffer-file-name))))
    (cond
     ((or (eq backend 'DARCS) (darcsum-repository-root default-directory))
      (darcsum-whatsnew default-directory))
     ((eq backend 'Bzr)
      ;; vc gets confused when running status on sibling bzr dirs
      (vc-dir (locate-dominating-file default-directory ".bzr")))
     ((eq backend 'Git)
      (magit-status-internal (magit-toplevel-safe default-directory)))
     (backend
      (vc-dir default-directory))
     (t
      (message "No recognized VC repository in sight")))))

(defun esk/compile-next-makefile (command)
  "Run a compilation after changing the working directory"
  (interactive
   (list
    (let ((command (eval compile-command)))
      (if (or compilation-read-command current-prefix-arg)
          (compilation-read-command command)
        command))))
  (let* ((root-dir (or (locate-dominating-file default-directory "Makefile") "."))
         (cd-command (concat "cd " root-dir " && " command)))
    (compile cd-command)
    (setq compile-command command)))

(defvar virtualenv-old-path)
(defvar virtualenv-old-exec-path)

(defun esk/virtualenv-activate (dir)
  "Activate the virtualenv located in DIR."

  ;; Removing the eventually present trailing slash
  (when (string= (substring dir -1 nil) "/")
    (setq dir (substring dir 0 -1)))

  ;; Eventually deactivate previous virtualenv
  (when virtualenv-name
    (virtualenv-deactivate))

  ;; Storing old variables
  (setq virtualenv-old-path (getenv "PATH"))
  (setq virtualenv-old-exec-path exec-path)

  (setq virtualenv-name (file-name-nondirectory dir))

  ;; I usually have the concrete virtual env isolated in a "env"
  ;; subdirectory, so use that if it exists.
  (if (file-exists-p (concat dir "/env/bin"))
      (setq dir (concat dir "/env")))

  (setenv "VIRTUAL_ENV" dir)
  (virtualenv-add-to-path (concat dir "/bin"))
  (add-to-list 'exec-path (concat dir "/bin"))

  (message (concat "Virtualenv '" virtualenv-name "' activated.")))

(defun esk/activate-virtual-desktop ()
  "Turn on a virtualenv and its related desktop, in auto-save mode"
  (interactive)

  (require 'virtualenv)

  ;; Eventually deactivate current desktop
  (when desktop-save-mode
    (virtualenv-deactivate)
    (desktop-kill))

  (let ((dir (ido-read-directory-name "Virtual desktop: ")))
    (esk/virtualenv-activate dir)
    (setq desktop-base-file-name "emacs.desktop")
    (setq desktop-dirname dir)
    (setq desktop-save t)
    (setq desktop-save-mode t)
    (unless (desktop-read dir)
      (dired dir))
    (setq server-name (md5 dir)))
  (server-start))

(defun esk/sort-words (reverse beg end)
  "Sort words in region alphabetically, in REVERSE if negative.
Prefixed with negative \\[universal-argument], sorts in reverse.

The variable `sort-fold-case' determines whether alphabetic case
affects the sort order.

See `sort-regexp-fields'."
  (interactive "P\nr")
  (sort-regexp-fields reverse "\\w+" "\\&" beg end))

;; Cycle thru a ring of dictionaries

;; Uncomment/add desired languages. Alternatively, specific users may
;; augment the ring adding something like
;;
;;  (ring-insert+extend lang-ring "castellano8" t)
;;
;; to their user.el file

(let ((langs '("american"
               "italiano"
               ; "castellano8"
               ; "deutsch8"
               ; "francais"
               ; "brasileiro"
               )))
  (setq lang-ring (make-ring (length langs)))
  (dolist (elem langs) (ring-insert lang-ring elem)))

(defun esk/cycle-ispell-languages ()
  (interactive)
  (let ((lang (ring-ref lang-ring -1)))
    (ring-insert lang-ring lang)
    (ispell-change-dictionary lang)))

(defun esk/rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(defun esk/delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(defun esk/quote-symbol (quote reverse)
  "Wrap symbol at point (previous if REVERSE) between `quote` chars."
  (if reverse (forward-symbol -1) (forward-symbol 1))
  (insert-char quote)
  (if reverse (forward-symbol 1) (forward-symbol -1))
  (insert-char quote))

(defun esk/single-quote-symbol (&optional reverse)
  "Wrap symbol at point (previous if REVERSE) between single quote chars."
  (interactive "P")
  (esk/quote-symbol ?\' reverse))

(defun esk/single-quote-symbol-behind ()
  "Wrap previous symbol between single quote chars."
  (interactive)
  (esk/quote-symbol ?\' 1))

(defun esk/double-quote-symbol (&optional reverse)
  "Wrap symbol at point (previous if REVERSE) between double quote chars."
  (interactive "P")
  (esk/quote-symbol ?\" reverse))

(defun esk/double-quote-symbol-behind ()
  "Wrap previous symbol between double quote chars."
  (interactive)
  (esk/quote-symbol ?\" 1))

(defun esk/move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (beginning-of-line)
    (when (or (> arg 0) (not (bobp)))
      (forward-line)
      (when (or (< arg 0) (not (eobp)))
        (transpose-lines arg))
      (forward-line (if (< arg 0) -2 -1))))))

(defun esk/move-text-down (arg)
  "Move region (transient-mark-mode active) or current line arg lines down."
  (interactive "*p")
  (esk/move-text-internal arg))

(defun esk/move-text-up (arg)
  "Move region (transient-mark-mode active) or current line arg lines up."
  (interactive "*p")
  (esk/move-text-internal (- arg)))

(defun esk/open-next-line (arg)
  "Move to the next line and then open a line."
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (forward-line 1)
  (indent-according-to-mode))

(defun esk/open-previous-line (arg)
  "Open a new line before the current one."
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (indent-according-to-mode))

(unless (fboundp 'backward-symbol)
  (defun backward-symbol (&optional arg)
    "Move backward until encountering the beginning of a word.
With argument ARG, do this that many times.
If ARG is omitted or nil, move point backward one word."
    (interactive "^p")
    (forward-symbol (- (or arg 1)))))

(defun esk/transpose-symbols (arg)
  "Interchange symbols around point, leaving point at end of them.
With prefix arg ARG, effect is to take word before or around point
and drag it forward past ARG other words (backward if ARG negative).
If ARG is zero, the words around or after point and around or after mark
are interchanged."
  (interactive "*p")
  (transpose-subr #'forward-symbol arg))
