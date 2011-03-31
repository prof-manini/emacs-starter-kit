;; starter-kit-python.el - setup of python stuff
(require 'pymacs (concat dotfiles-dir "elpa-to-submit/pymacs.el"))

(defun setup-ropemacs ()
  "Setup the ropemacs harness"
  (setenv "PYTHONPATH"
          (concat
           (getenv "PYTHONPATH") path-separator
           (concat dotfiles-dir "python-libs/")))
  (pymacs-load "ropemacs" "rope-")

  ;; Stops from erroring if there's a syntax err
  (setq ropemacs-codeassist-maxfixes 3)
  (setq ropemacs-guess-project t)
  (setq ropemacs-enable-autoimport t)
  )

(defun ropemacs-auto-open-project ()
  "Automatically open a rope project if there is one"
  (let* ((project-dir (locate-dominating-file default-directory ".ropeproject")))
    (if (and project-dir (file-exists-p project-dir))
        (rope-open-project project-dir)))
  )

;; Customizing
(setq flymake-enable-pyflakes t)
(setq flymake-enable-pylint nil)
(setq flymake-enable-pep8 nil)

;; Python or python mode?
(eval-after-load 'python-mode
  '(progn
     ;;==================================================
     ;; Ropemacs Configuration
     ;;==================================================
     ;;(setup-ropemacs)

     ;;==================================================
     ;; Virtualenv Commands
     ;;==================================================
     (autoload 'virtualenv-activate "virtualenv"
       "Activate a Virtual Environment specified by PATH" t)

     (autoload 'virtualenv-workon "virtualenv"
       "Activate a Virtual Environment present using virtualenvwrapper" t)

     ;;==================================================
     ;; Flymake for python configuration
     ;;===================================================

     ;; TODO: There is some duplication, that can be removed using macros
     ;; TODO: Implement flymake-remove-checker
     ;; Instructions to add a new checker based on command:
     ;;
     ;; 1) Write an init function, the flymake-command-setup performs some
     ;;    checks and at the end of the option list the filename to process:
     ;;
     ;;   (defun flymake-newchecker-init ()
     ;;      (flymake-command-setup "command" (list "option1" "option2")))
     ;;
     ;; 2) Use the flymake-add-checker function
     ;;
     ;;    (flymake-add-checker flymake-newchecker-init)

     (require 'tramp)
     ;; Utilities that increase legibility and reduce code duplication
     (defun current-file-remotep ()
       "Tell if the file is remote"
       (subsetp (list (current-buffer)) (tramp-list-remote-buffers)))

     (defun flymake-create-copy-file ()
       "Create a copy local file"
       (let* ((temp-file (flymake-init-create-temp-buffer-copy
                          'flymake-create-temp-inplace)))
         (file-relative-name
          temp-file
          (file-name-directory buffer-file-name))))

     (defun flymake-command-setup (command &optional options)
       "Setup the command to be used with flymake, the command
will be called in this way: COMMAND OPTIONS FILE The FILE varible
is passed after the options."
       ;; Make sure it's not a remote buffer or flymake would not work
       (when (not (current-file-remotep))
         (list command
               (append options (list (flymake-create-copy-file))))))

     (when (require 'flymake "flymake-patch" t)
       (setq flymake-info-line-regex
             (append flymake-info-line-regex '("unused$" "^redefinition" "used$"))))

     ;; I'm using individual well-defined names to be able to remove them
     ;; in some way

     ;; Init functions!
     (defun flymake-pyflakes-init ()
       (flymake-command-setup "pyflakes"))

     (defun flymake-pep8-init ()
       (flymake-command-setup "pep8"))

     (defun flymake-pylint-init ()
       (flymake-command-setup "python" (list (concat dotfiles-dir "scripts/pylint-mod.py"))))

     (defun flymake-disable-python-checkers ()
       "Disable all python checkers"
       (dolist (flymake-checker-init '(flymake-pyflakes-init flymake-pep8-init flymake-pylint-init))
         (remove '("\\.py\\'" flymake-checker-init) 'flymake-allowed-file-name-masks)))

     (defun flymake-add-checker (command)
       "Add the checker specified by the COMMAND list"
       (add-to-list 'flymake-allowed-file-name-masks
                    (list "\\.py\\'" command)))

     ;; Not on all modes, please
     (add-hook 'python-mode-hook 'flymake-find-file-hook)

     (when flymake-enable-pyflakes
       (flymake-add-checker 'flymake-pyflakes-init))

     (when flymake-enable-pylint
       (flymake-add-checker 'flymake-pylint-init))

     (when flymake-enable-pep8
       (flymake-add-checker 'flymake-pep8-init)))
  )

;; Cython Mode
(autoload 'cython-mode "cython-mode" "Mode for editing Cython source files")

(add-to-list 'auto-mode-alist '("\\.pyx\\'" . cython-mode))
(add-to-list 'auto-mode-alist '("\\.pxd\\'" . cython-mode))
(add-to-list 'auto-mode-alist '("\\.pxi\\'" . cython-mode))

;; eshell - see http://wiki.zope.org/klm/PDBTrack and
;;          http://www.emacswiki.org/emacs/EshellAndPdbTrack

(require 'python-mode)

(add-hook 'eshell-output-filter-functions
       '(lambda () ""
          (when (eshell-interactive-process)
            (py-pdbtrack-track-stack-file
             (buffer-substring (eshell-beginning-of-output) (eshell-end-of-output))))))

; Redefine the following function from python-mode.el: use
; eshell-last-xxx-end instead of comint-last-xxx-end

(defun py-pdbtrack-track-stack-file (text)
  "Show the file indicated by the pdb stack entry line, in a separate window.

Activity is disabled if the buffer-local variable
`py-pdbtrack-do-tracking-p' is nil.

We depend on the pdb input prompt matching `py-pdbtrack-input-prompt'
at the beginning of the line.

If the traceback target file path is invalid, we look for the most
recently visited python-mode buffer which either has the name of the
current function \(or class) or which defines the function \(or
class).  This is to provide for remote scripts, eg, Zope's 'Script
(Python)' - put a _copy_ of the script in a buffer named for the
script, and set to python-mode, and pdbtrack will find it.)"
  ;; Instead of trying to piece things together from partial text
  ;; (which can be almost useless depending on Emacs version), we
  ;; monitor to the point where we have the next pdb prompt, and then
  ;; check all text from comint-last-input-end to process-mark.
  ;;
  ;; Also, we're very conservative about clearing the overlay arrow,
  ;; to minimize residue.  This means, for instance, that executing
  ;; other pdb commands wipe out the highlight.  You can always do a
  ;; 'where' (aka 'w') command to reveal the overlay arrow.
  (let* ((origbuf (current-buffer))
         (currproc (get-buffer-process origbuf)))

    (if (not (and currproc py-pdbtrack-do-tracking-p))
        (py-pdbtrack-overlay-arrow nil)

      (let* ((procmark (point))
             (block (buffer-substring (max eshell-last-input-end
                                           (- procmark
                                              py-pdbtrack-track-range))
                                      procmark))
             target target_fname target_lineno target_buffer)

        (if (not (string-match (concat py-pdbtrack-input-prompt "$") block))
            (py-pdbtrack-overlay-arrow nil)

          (setq target (py-pdbtrack-get-source-buffer block))

          (if (stringp target)
              (message "pdbtrack: %s" target)

            (setq target_lineno (car target))
            (setq target_buffer (cadr target))
            (setq target_fname (buffer-file-name target_buffer))
            (switch-to-buffer-other-window target_buffer)
            (goto-line target_lineno)
            (message "pdbtrack: line %s, file %s" target_lineno target_fname)
            (py-pdbtrack-overlay-arrow t)
            (pop-to-buffer origbuf t)

            )))))
  )

(provide 'starter-kit-python)
