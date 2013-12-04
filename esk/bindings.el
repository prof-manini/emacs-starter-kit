;;; starter-kit-bindings.el --- Set up some handy key bindings

;; You know, like Readline.
(global-set-key (kbd "C-M-h") 'backward-kill-word)

;; Align your code in a pretty way.
(global-set-key (kbd "C-x \\") 'align-regexp)

;; Completion that uses many different methods to find options.
(global-set-key (kbd "M-/") 'hippie-expand)

;; Font size
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

;; Use regex searches by default.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "\C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; Jump to a definition in the current file. (This is awesome.)
(global-set-key (kbd "M-TAB") 'ido-imenu)

;; File finding
(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
(global-set-key (kbd "C-x C-M-f") 'find-file-in-project)
(global-set-key (kbd "C-x f") 'recentf-ido-find-file)
(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "M-`") 'file-cache-minibuffer-complete)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Window switching. (C-x o goes to the next window)
(windmove-default-keybindings) ;; Shift+direction
(global-set-key (kbd "C-x O") (lambda () (interactive) (other-window -1))) ;; back one
(global-set-key (kbd "C-x C-o") (lambda () (interactive) (other-window 2))) ;; forward two
(global-set-key (kbd "C-c s") 'toggle-window-split)

;; Activate global winner mode: [C-c left] will restore previous
;; window configuration
(winner-mode 1)

;; If you want to be able to M-x without meta (phones, etc)
(global-set-key (kbd "C-x C-m") 'execute-extended-command)

;; Fetch the contents at a URL, display it raw.
(global-set-key (kbd "C-x C-h") 'view-url)

;; Help should search more than just commands
(global-set-key (kbd "C-h a") 'apropos)

;; Should be able to eval-and-replace anywhere.
(global-set-key (kbd "C-c e") 'eval-and-replace)

;; This is a little hacky since VC doesn't support git add internally
;; (eval-after-load 'vc
;;   (define-key vc-prefix-map "i" '(lambda () (interactive)
;;                                    (if (not (eq 'Git (vc-backend buffer-file-name)))
;;                                        (vc-register)
;;                                      (shell-command (format "git add %s" buffer-file-name))
;;                                      (message "Staged changes.")))))

;; Activate occur easily inside isearch
(define-key isearch-mode-map (kbd "C-o")
  (lambda () (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string (regexp-quote isearch-string))))))

;; Org
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

;; Function keys bindings
(global-set-key [f1] 'hippie-expand)   ; alias di M-/

(global-set-key [f2] 'query-replace)    ; alias di M-%
(global-set-key [S-f2] 'query-replace-regexp)

(global-set-key [f3] 'grep)
(global-set-key [S-f3] 'grep-find)

(global-set-key [f4] 'kmacro-end-or-call-macro)
(global-set-key [S-f4] 'kmacro-start-macro-or-insert-counter)
(global-set-key [M-f4] 'kmacro-edit-macro-repeat)

(global-set-key [f5] 'compile-next-makefile)

(global-set-key [f6] 'next-error)
(global-set-key [S-f6] 'previous-error)

(global-set-key [f7] 'flymake-goto-next-error)
(global-set-key [S-f7] 'flymake-goto-prev-error)

(global-set-key [f8] 'cycle-ispell-languages)

(global-set-key [f9] 'whatsnew-or-magit-status-or-vc-dir)
(global-set-key [S-f9] 'magit-blame-mode)

(global-set-key [f10] 'eshell)
(global-set-key [S-f10] 'shell)
(global-set-key [M-f10] (lambda () (interactive) (eshell t))) ; Start a new eshell even if one is active

(global-set-key [f11] 'activate-virtual-desktop)

(global-set-key [f12] 'auto-fill-mode)
(global-set-key [S-f12] 'linum-mode)
(global-set-key [M-f12] 'menu-bar-mode)

;; Other global keys
(global-set-key [home] 'beginning-of-line)
(global-set-key [end] 'end-of-line)
(global-set-key [C-home] 'beginning-of-buffer)
(global-set-key [C-end] 'end-of-buffer)
(global-set-key [delete] 'delete-char)

(global-set-key [C-left] 'backward-word)
(global-set-key [C-right] 'forward-word)

(global-set-key [M-left] 'previous-buffer)
(global-set-key [M-right] 'next-buffer)

(add-hook 'prog-mode-hook (lambda () (local-set-key "\C-m" 'newline-and-indent)))

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR.")
(global-set-key (kbd "M-Z") 'zap-up-to-char)

(global-set-key (kbd "C-\'") 'single-quote-word)
(global-set-key (kbd "M-\'") 'single-quote-word-behind)

(global-set-key (kbd "C-\"") 'double-quote-word)
(global-set-key (kbd "M-\"") 'double-quote-word-behind)

(global-set-key (kbd "<C-S-down>") 'move-text-down)
(global-set-key (kbd "<C-S-up>") 'move-text-up)

(global-set-key (kbd "C-o") 'open-next-line)
(global-set-key (kbd "M-o") 'open-previous-line)