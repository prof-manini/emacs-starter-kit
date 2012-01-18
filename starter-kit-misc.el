;;; starter-kit-misc.el --- Things that don't fit anywhere else
;;
;; Part of the Emacs Starter Kit

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
      whitespace-global-modes '(emacs-lisp-mode
                                js-mode
                                makefile-mode
                                python-mode)
      ediff-window-setup-function 'ediff-setup-windows-plain
      xterm-mouse-mode t
      save-place-file (concat esk-dotfiles-dir "places")
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

;; Save a list of recent files visited.
(setq recentf-max-saved-items 30)
(recentf-mode 1)

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

;; Matching parentheses for all languages and so on
(autopair-global-mode t)
(setq autopair-autowrap t)
;; Fix for triple quotes in python
(add-hook 'python-mode-hook
          #'(lambda ()
              (setq autopair-handle-action-fns
                    (list #'autopair-default-handle-action
                          #'autopair-python-triple-quote-action))))

;; ido-mode is like magic pixie dust!
(when (> emacs-major-version 21)
  (ido-mode t)
  (setq ido-enable-prefix nil
        ido-enable-flex-matching t
        ido-create-new-buffer 'always
        ido-use-filename-at-point nil
        ido-max-prospects 10
        ido-ignore-files '("\\`#" "\\`.#" "\\`\\.\\./" "\\`\\./" "\\.orig\\'")
        ido-ignore-directories '("\\`\\.\\./" "\\`\\./")
        ido-auto-merge-work-directories-length -1
        ))

;; but please use a better ordering, when dealing with Pyjamas for example...
(setq ido-file-extensions-order '(".py" ".js" t))

;; ignore some more directory patterns
(add-to-list 'completion-ignored-extensions ".egg-info/")

;; turn on global whitespace handling
(global-whitespace-mode t)

;; by default do not use TABs
(set-default 'indent-tabs-mode nil)

;; but Makefiles are an exception, TAB is mandatory at bol
(add-hook 'makefile-mode-hook
          #'(lambda ()
              (setq indent-tabs-mode t)
              (add-to-list (make-local-variable 'whitespace-style) 'indentation::tab)
              (add-hook 'before-save-hook 'whitespace-cleanup)
              ))

(set-default 'indicate-empty-lines t)
(set-default 'imenu-auto-rescan t)

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-flyspell)

(defvar coding-hook nil
  "Hook that gets run on activation of any programming mode.")

(defalias 'yes-or-no-p 'y-or-n-p)
(random t) ;; Seed the random-number generator

;; Hippie expand: at times perhaps too hip
(delete 'try-expand-line hippie-expand-try-functions-list)
(delete 'try-expand-list hippie-expand-try-functions-list)

;; Activate file backups
(setq version-control t)
(setq trim-versions-without-ask t)
(setq delete-old-versions t)
(setq backup-by-copying-when-linked t)

;; Don't clutter up directories with files~
(setq backup-directory-alist `(("." . ,(expand-file-name
                                        (concat esk-dotfiles-dir "backups")))))

;; nxhtml stuff
(setq mumamo-chunk-coloring 'submode-colored
      nxhtml-skip-welcome t
      indent-region-mode t
      rng-nxml-auto-validate-flag nil)

;; Associate modes with file extensions

(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG$" . diff-mode))
(add-to-list 'auto-mode-alist '("\\.css$" . css-mode))
(add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.js\\(on\\)?$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.\\(pt\\|xml\\|xsl\\|rng\\|xhtml\\|zcml\\)\\'" . nxml-mode))

(autoload 'rst-mode "rst")
(add-to-list 'auto-mode-alist '("\\.rst$" . rst-mode))

(autoload 'po-mode "po-mode")
(add-to-list 'auto-mode-alist '("\\.po[tx]?\\'\\|\\.po\\." . po-mode))
(modify-coding-system-alist 'file "\\.po[tx]?\\'\\|\\.po\\." 'po-find-file-coding-system)

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

(eval-after-load 'mumamo
  '(eval-after-load 'zenburn
     '(ignore-errors (set-face-background
                      'mumamo-background-chunk-submode "gray22"))))

;; Platform-specific stuff
(when (eq system-type 'darwin)
  ;; Work around a bug on OS X where system-name is FQDN
  (setq system-name (car (split-string system-name "\\."))))

;; make emacs use the clipboard
(setq x-select-enable-clipboard t)

;; Yasnippet setup
(yas/initialize)
(yas/load-directory (concat esk-dotfiles-dir "elpa-to-submit/yasnippet/snippets"))
(setq yas/prompt-functions '(yas/dropdown-prompt yas/ido-prompt yas/x-prompt))
(setq yas/wrap-around-region 'cua)

(provide 'starter-kit-misc)
;;; starter-kit-misc.el ends here
