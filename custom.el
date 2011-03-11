;; -*- Emacs-Lisp -*-
;;:Progetto:  dot.emacs -- Impostazioni principali, preferenze generali
;;:Creato il: Mon Feb  2 19:37:56 2004
;;

; Attiva le funzionalità di version control
(setq version-control t)

; Elimina le vecchie versioni senza chiedere
(setq trim-versions-without-ask t)
(setq delete-old-versions t)
(setq backup-by-copying-when-linked t)

; Fai vedere le righe vuote
(setq indicate-empty-lines t)

; Mantieni la posizione negli scroll
(setq scroll-preserve-screen-position t)

; Evidenzia la selezione
(transient-mark-mode t)

(put 'downcase-region 'disabled nil)

; Abilita la valutazione delle variabili locali
(setq enable-local-eval t)

; Non inserire mai dei tabulatori per l'indentazione
(setq-default indent-tabs-mode nil)

; Non inserire spazi intorno alle parentesi (scrivo poco lisp :)
(setq parens-require-spaces nil)

;; Show scrollbars on the right
(set-scroll-bar-mode 'right)

; Override the default impl, that does also a (indent-buffer)
(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (untabify-buffer)
  (whitespace-cleanup))

(defun whatsnew-or-vc-dir ()
  "Run either darcsum-whatsnew or vc-dir accordingly with current vc-backend"
  (interactive)
  (let ((backend (vc-backend (buffer-file-name))))
    (if (eq backend 'DARCS)
        (darcsum-whatsnew default-directory)
      (vc-dir default-directory)
      )
    )
  )

(defun compile-next-makefile (command)
  "Run a compilation after changing the working directory"
  (interactive
   (list
    (let ((command (eval compile-command)))
      (if (or compilation-read-command current-prefix-arg)
          (compilation-read-command command)
        command))))
  (let* ((root-dir (or (locate-dominating-file default-directory "Makefile") "."))
         (cd-command (concat "cd " root-dir " && " command)))
    (compile cd-command)))

(defun turn-on-desktop ()
  "Turn on nearest desktop, in auto-save mode"
  (interactive)
  (if desktop-save-mode
      (message "Already active")
    (setq desktop-base-file-name "emacs.desktop")
    (setq desktop-dirname (or (locate-dominating-file default-directory "emacs.desktop")
                              default-directory))
    (setq desktop-save t)
    (setq desktop-save-mode t)
    (desktop-read desktop-dirname)
    )
  )

;;
;; Function keys bindings
;;

(global-set-key [f1] 'hippie-expand)   ; alias di M-/
(global-set-key [f2] 'query-replace)    ; alias di M-%
(global-set-key [S-f2] 'query-replace-regexp)
(global-set-key [f3] 'grep)
(global-set-key [S-f3] 'grep-find)
(global-set-key [C-f3] 'compile-next-makefile)
(global-set-key [f4] 'next-error)
(global-set-key [S-f4] 'previous-error)

(global-set-key [f5] 'call-last-kbd-macro)
(global-set-key [f6] 'name-last-kbd-macro)
(global-set-key [f7] 'edit-named-kbd-macro)
(global-set-key [f8] 'cleanup-buffer)

(global-set-key [f9] 'whatsnew-or-vc-dir)
(global-set-key [f10] 'shell)
(global-set-key [S-f10] 'eshell)
(global-set-key [f11] 'turn-on-desktop)
(global-set-key [f12] 'auto-fill-mode)

(global-set-key [home] 'beginning-of-line)
(global-set-key [end] 'end-of-line)
(global-set-key [C-home] 'beginning-of-buffer)
(global-set-key [C-end] 'end-of-buffer)
(global-set-key "\C-m" 'newline-and-indent)
(global-set-key [delete] 'delete-char)

(global-set-key [C-left] 'backward-word)
(global-set-key [C-right] 'forward-word)

(global-set-key [M-left] 'previous-buffer)
(global-set-key [M-right] 'next-buffer)

;;
;; Modes
;;

;; Python, a must
(require 'python-mode)

;; Adding hook to automatically open a rope project if there is one
;; in the current or in the upper level directory: this may be too
;; heavy to be the default
;; (add-hook 'python-mode-hook 'ropemacs-auto-open-project)

;; remove the rope hooks for now
(remove-hook 'python-mode-hook 'ac-python-mode-setup)
(remove-hook 'rope-open-project-hook 'ac-nropemacs-setup)

;; gettext
(require 'po)
(setq auto-mode-alist
      (cons '("\\.po[tx]?\\'\\|\\.po\\." . po-mode) auto-mode-alist))
(autoload 'po-mode "po-mode")

;; nxml
(setq auto-mode-alist
      (cons '("\\.\\(pt\\|xml\\|xsl\\|rng\\|xhtml\\|zcml\\)\\'" . nxml-mode)
            auto-mode-alist))

;; reStructuredText
(require 'rst)

;; attiva la modalità rst sui file *.rst
(setq auto-mode-alist (cons '("\\.rst$" . rst-mode) auto-mode-alist))

;; aggiorna automaticamente il contents
(add-hook 'rst-adjust-hook 'rst-toc-update)

;; disabilita il font-lock dei titoli, dei code-blocks... che rallenta
;; troppo!
(setq rst-mode-lazy nil)
(setq rst-directive-face 'font-lock-builtin-face)

;; Attiva i bindings standard (vedi C-c p)
;;(add-hook 'rst-mode-hook 'rst-text-mode-bindings)
