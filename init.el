;;; init.el --- Where all the magic begins
;;
;; Part of the Emacs Starter Kit
;;
;; This is the first thing to get loaded.
;;
;; "Emacs outshines all other editing software in approximately the
;; same way that the noonday sun does the stars. It is not just bigger
;; and brighter; it simply makes everything else vanish."
;; -Neal Stephenson, "In the Beginning was the Command Line"

;; Turn off mouse interface early in startup to avoid momentary display
;; You really don't need these; trust me.
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; by default do not use TABs
(set-default 'indent-tabs-mode nil)


;;; Common variables

(setq

 ;; top directory, usually ~/.emacs.d/
 esk-top-dir (file-name-directory (or (buffer-file-name) load-file-name))

 ;; starter kit lisp sources directory
 esk-lisp-dir (concat esk-top-dir "esk/")

 ;; not-yet-packaged-packages directory
 esk-autoload-dir (concat esk-top-dir "elpa-to-submit/")

 ;; directory where user specific stuff go
 esk-user-specific-dir (concat esk-top-dir user-login-name "/")

 ;; directory containing overrides
 esk-overrides-dir (concat esk-top-dir "overrides/")

 ;; user specific configuration file
 esk-user-specific-config (concat esk-top-dir user-login-name))


;;; Load up ELPA, the package manager

;; directory where emacs packages are installed
(setq package-user-dir (concat esk-top-dir "elpa/"))

(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)

(package-initialize)

(load (concat esk-lisp-dir "elpa"))

;; These should be loaded on startup rather than autoloaded on demand
;; since they are likely to be used in every session

(require 'cl)
(require 'ffap)
(require 'uniquify)
(require 'ansi-color)
(require 'tramp)

;; Autoloaded stuff

(add-to-list 'load-path esk-autoload-dir)
(add-to-list 'load-path (concat esk-autoload-dir "magit"))
(add-to-list 'load-path (concat esk-autoload-dir "git-modes"))

(defun esk-regen-autoloads (&optional force-regen)
  "Regenerate the autoload definitions file if necessary and load it."
  (interactive "P")
  (let ((generated-autoload-file (concat esk-autoload-dir "loaddefs.el")))
    (when (or force-regen
              (not (file-exists-p generated-autoload-file))
              (cl-some (lambda (f)
                         (file-newer-than-file-p f generated-autoload-file))
                    (directory-files esk-autoload-dir t "\\.el$")))
      (message "Updating autoload file %s..." generated-autoload-file)
      (let (emacs-lisp-mode-hook)
        (update-directory-autoloads esk-autoload-dir)))
    (load generated-autoload-file)))

(esk-regen-autoloads)

;; Load up starter kit customizations

(load (concat esk-lisp-dir "yasnippet"))
(load (concat esk-lisp-dir "defuns"))
(load (concat esk-lisp-dir "bindings"))
(load (concat esk-lisp-dir "misc"))
(load (concat esk-lisp-dir "registers"))
(load (concat esk-lisp-dir "eshell"))
(load (concat esk-lisp-dir "lisp"))
(load (concat esk-lisp-dir "js"))
(load (concat esk-lisp-dir "python"))
(load (concat esk-lisp-dir "completion"))
(if (locate-file "darcs" exec-path exec-suffixes 'file-executable-p)
    (load (concat esk-lisp-dir "darcs")))
(if (locate-file "git" exec-path exec-suffixes 'file-executable-p)
    (load (concat esk-lisp-dir "git")))
(load (concat esk-lisp-dir "erc"))
(load (concat esk-lisp-dir "jabber"))
(load (concat esk-lisp-dir "skeletons"))
(load (concat esk-lisp-dir "notmuch"))
(load (concat esk-lisp-dir "gnus"))

;; Load generic customizations
(load (concat esk-top-dir "custom") 'noerror)

;; Load system specific customizations
(load (concat esk-top-dir system-name) 'noerror)

;; Where emacs will write user custom settings
(setq custom-file (concat esk-user-specific-config ".el"))

;; Load user specific customizations
(load esk-user-specific-config 'noerror)

;; Overrides for possibly old bundled versions
(if (file-exists-p esk-overrides-dir)
    (add-to-list 'load-path esk-overrides-dir))

;; Automatically load all "user-name"/*.el files
(if (file-exists-p esk-user-specific-dir)
    (mapc #'load (directory-files esk-user-specific-dir nil ".*el$")))

;;; init.el ends here
