;;; starter-kit-misc.el --- Things that don't fit anywhere else
;;

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (mouse-wheel-mode t)
  (blink-cursor-mode -1)
  (set-scroll-bar-mode 'right))

(add-hook 'before-make-frame-hook 'turn-off-tool-bar)

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(ansi-color-for-comint-mode-on)

(setq visible-bell t
      echo-keystrokes 0.1
      enable-local-eval t
      font-lock-maximum-decoration t
      inhibit-startup-message t
      transient-mark-mode t
      color-theme-is-global t
      shift-select-mode nil
      parens-require-spaces nil
      require-final-newline t
      truncate-partial-width-windows nil
      uniquify-buffer-name-style 'forward
      whitespace-style '(face
                         trailing
                         lines-tail
                         empty
                         space-before-tab
                         space-after-tab
                         indentation
                         indentation::space
                         tabs)
      ediff-window-setup-function 'ediff-setup-windows-plain
      scroll-preserve-screen-position t)

(add-to-list 'safe-local-variable-values '(lexical-binding . t))
(add-to-list 'safe-local-variable-values '(whitespace-line-column . 80))

;; Set this to whatever browser you use
;; (setq browse-url-browser-function 'browse-url-firefox)
;; (setq browse-url-browser-function 'browse-default-macosx-browser)
;; (setq browse-url-browser-function 'browse-default-windows-browser)
;; (setq browse-url-browser-function 'browse-default-kde)
;; (setq browse-url-browser-function 'browse-default-epiphany)
;; (setq browse-url-browser-function 'browse-default-w3m)
;; (setq browse-url-browser-function 'browse-url-generic
;;       browse-url-generic-program "~/src/conkeror/conkeror")

;; Transparently open compressed files
(auto-compression-mode t)

;; Enable syntax highlighting for older Emacsen that have it off
(global-font-lock-mode t)

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

;; Matching parentheses for all languages and so on
(electric-pair-mode nil)

;; ido-mode is like magic pixie dust!
(ido-mode t)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-use-filename-at-point nil
      ido-max-prospects 10
      ido-ignore-files '("\\`#" "\\`.#" "\\.orig\\'"
                         "\\`\\.\\./" "\\`\\./" "\\`__pycache__/")
      ido-ignore-directories '("\\`\\.\\./" "\\`\\./" "\\`__pycache__/")
      ido-auto-merge-work-directories-length -1
      ido-file-extensions-order '(".py" ".js" t)
      )

;; ignore some more directory patterns
(add-to-list 'completion-ignored-extensions ".egg-info/")

;; Makefiles are an exception, TAB is mandatory at bol
(add-hook 'makefile-mode-hook 'turn-on-whitespace-mode-makefiles)
(add-hook 'makefile-gmake-mode-hook 'turn-on-whitespace-mode-makefiles)

(set-default 'indicate-empty-lines t)
(set-default 'imenu-auto-rescan t)

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-flyspell)

(defvar coding-hook nil
  "Hook that gets run on activation of any programming mode.")

;; Use a shorter answer
(defalias 'yes-or-no-p 'y-or-n-p)

;; Seed the random-number generator
(random t)

;; Activate file backups
(setq version-control t)
(setq trim-versions-without-ask t)
(setq delete-old-versions t)
(setq backup-by-copying-when-linked t)

;; Don't clutter up directories with files~
(setq backup-directory-alist `(("." . ,(expand-file-name
                                        (concat esk-top-dir "backups")))))

;; Associate modes with file extensions

(add-to-list 'auto-mode-alist '("\\.s?css$" . css-mode))
(add-to-list 'auto-mode-alist '("\\.js\\(on\\)?$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.\\(pt\\|xml\\|xsl\\|rng\\|xhtml\\|zcml\\)\\'" . nxml-mode))

(autoload 'rst-mode "rst")
(add-to-list 'auto-mode-alist '("\\.rst$" . rst-mode))

(add-to-list 'auto-mode-alist '("\\.po[tx]?\\'\\|\\.po\\." . po-mode))
(modify-coding-system-alist 'file "\\.po[tx]?\\'\\|\\.po\\." 'po-find-file-coding-system)

(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("\\.scss$" . scss-mode))

;; longlines-mode has been obsoleted by visual-line-mode, but the
;; latter does not do the right thing IMHO...
(unless (fboundp 'longlines-mode)
  (autoload 'longlines-mode "obsolete/longlines"))
(eval-after-load 'po-mode
  '(progn
     ;; Turn on and off longlines-mode to wrap when editing a message
     ;; and unwrap before putting it back.
     (add-hook 'po-subedit-mode-hook (lambda () (longlines-mode 1)))
     (add-hook 'po-subedit-exit-hook (lambda () (longlines-mode 0)))))

(eval-after-load 'grep
  '(when (boundp 'grep-find-ignored-files)
    (add-to-list 'grep-find-ignored-files "target")
    (add-to-list 'grep-find-ignored-files "*.class")))

;; Default to unified diffs
(setq diff-switches "-u")

;; Cosmetics

(set-face-background 'vertical-border "white")
(set-face-foreground 'vertical-border "white")

(eval-after-load 'diff-mode
  '(progn
     (set-face-foreground 'diff-added "green4")
     (set-face-foreground 'diff-removed "red3")))

;; Platform-specific stuff
(when (eq system-type 'darwin)
  ;; Work around a bug on OS X where system-name is FQDN
  (setq system-name (car (split-string system-name "\\."))))

;; make emacs use the clipboard
(setq x-select-enable-clipboard t)

(add-hook 'css-mode-hook 'run-coding-hook)
(add-hook 'html-mode-hook 'run-coding-hook)
(add-hook 'rst-mode-hook 'run-coding-hook)
