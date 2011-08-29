;; starter-kit-python.el - setup of python stuff
(require 'pymacs (concat esk-dotfiles-dir "elpa-to-submit/pymacs.el"))

(defun setup-ropemacs ()
  "Setup the ropemacs harness"
  (setenv "PYTHONPATH"
          (concat
           (getenv "PYTHONPATH") path-separator
           (concat esk-dotfiles-dir "python-libs/")))
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

     (require 'tramp-cmds)
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
       (flymake-command-setup "python" (list (concat esk-dotfiles-dir "scripts/pylint-mod.py"))))

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
       (flymake-add-checker 'flymake-pep8-init))

     ;;
     ;; Additional functionality that makes flymake error messages appear
     ;; in the minibuffer when point is on a line containing a flymake
     ;; error. This saves having to mouse over the error, which is a
     ;; keyboard user's annoyance

     ;;flymake-ler(file line type text &optional full-file)
     (defun show-fly-err-at-point ()
       "If the cursor is sitting on a flymake error, display the
message in the minibuffer"
       (interactive)
       (let ((line-no (line-number-at-pos)))
         (dolist (elem flymake-err-info)
           (if (eq (car elem) line-no)
               (let ((err (car (second elem))))
                 (message "%s" (fly-pyflake-determine-message err)))))))

     (defun fly-pyflake-determine-message (err)
       "pyflake is flakey if it has compile problems, this adjusts the
message to display, so there is one ;)"
       (cond ((not (or (eq major-mode 'Python) (eq major-mode 'python-mode) t)))
             ((null (flymake-ler-file err))
              ;; normal message do your thing
              (flymake-ler-text err))
             (t ;; could not compile err
              (format "compile error, problem on line %s" (flymake-ler-line err)))))

     (defadvice flymake-goto-next-error (after display-message activate compile)
       "Display the error in the mini-buffer rather than having to mouse over it"
       (show-fly-err-at-point))

     (defadvice flymake-goto-prev-error (after display-message activate compile)
       "Display the error in the mini-buffer rather than having to mouse over it"
       (show-fly-err-at-point))

     (defadvice flymake-mode (before post-command-stuff activate compile)
       "Add functionality to the post command hook so that if the
cursor is sitting on a flymake error the error information is
displayed in the minibuffer (rather than having to mouse over
it)"
       (set (make-local-variable 'post-command-hook)
            (cons 'show-fly-err-at-point post-command-hook)))

     ;; Bind Python specific linters
     (define-key py-mode-map [S-f5] 'python-pylint)
     (define-key py-mode-map [M-f5] 'python-pep8)))

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

;; this makes things painfully slow, to be investigated
; (add-hook 'python-mode-hook 'run-coding-hook)

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
\(Python)' - put a _copy_ of the script in a buffer named for the
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
             (last-input-end (or (and (boundp 'eshell-last-input-end)
                                      eshell-last-input-end)
                                 comint-last-input-end))
             (block (buffer-substring (max last-input-end
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
            (goto-char (point-min))
            (forward-line (1- target_lineno))
            (message "pdbtrack: line %s, file %s" target_lineno target_fname)
            (py-pdbtrack-overlay-arrow t)
            (pop-to-buffer origbuf t)

            )))))
  )

(provide 'starter-kit-python)
