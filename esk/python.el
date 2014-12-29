;;; esk/python.el --- setup of python stuff
;;

(require 'python)

(add-hook 'python-mode-hook #'esk/run-coding-hook)

(require 'flymake-python-pyflakes)
(require 'flymake-cursor)

(add-hook 'python-mode-hook #'flymake-python-pyflakes-load)

(define-key python-mode-map (kbd "C-c b") #'python-nav-backward-defun)
(define-key python-mode-map (kbd "C-c f") #'python-nav-forward-defun)
(define-key python-mode-map (kbd "C-c u") #'python-nav-backward-statement)
(define-key python-mode-map (kbd "C-c d") #'python-nav-forward-statement)
