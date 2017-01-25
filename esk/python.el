;;; esk/python.el --- setup of python stuff
;;

(eval-when-compile
  (require 'python))

(defun esk/python-region-as-new-variable ()
  "Create a new variable, just before current statement, initialized to current region."
  (interactive)
  (let ((text (delete-and-extract-region (point) (mark)))
        (name (read-string "Variable name: " nil nil "varname")))
    (insert name)
    (python-nav-beginning-of-statement)
    (esk/open-previous-line 1)
    (insert name " = " text)))

(eval-after-load 'python
  '(progn
     (add-hook 'python-mode-hook #'esk/run-coding-hook)

     ;; Avoid pointless warning
     (csetq python-indent-guess-indent-offset-verbose nil)

     ;; Activate pdbtrack in M-x shell buffers
     (add-hook 'comint-output-filter-functions #'python-pdbtrack-comint-output-filter-function)

     (require 'flymake-python-pyflakes)
     (require 'flymake-cursor)

     (add-hook 'python-mode-hook #'flymake-python-pyflakes-load)
     (add-hook 'python-mode-hook
               (lambda ()
                 (setq-local prettify-symbols-alist '(("lambda" . ?λ)))))

     (require 'company-jedi)

     (add-to-list 'company-backends 'company-jedi)

     (define-key python-mode-map (kbd "C-c b") #'python-nav-backward-defun)
     (define-key python-mode-map (kbd "C-c f") #'python-nav-forward-defun)
     (define-key python-mode-map (kbd "C-c u") #'python-nav-backward-statement)
     (define-key python-mode-map (kbd "C-c d") #'python-nav-forward-statement)
     (define-key python-mode-map (kbd "C-c r") #'py-isort-buffer)
     (define-key python-mode-map (kbd "C-c v") #'esk/python-region-as-new-variable)
     (define-key python-mode-map (kbd "C-c S") #'esk/sort-symbols)))

(eval-after-load 'py-isort
  '(progn
     ;; --multi_line_output
     (csetq py-isort-options '("-m 3"))))
