;;; esk/python.el --- setup of python stuff
;;

(eval-after-load 'python
  '(progn
     (add-hook 'python-mode-hook #'esk/run-coding-hook)

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
     (define-key python-mode-map (kbd "C-c d") #'python-nav-forward-statement)))
