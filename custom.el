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
