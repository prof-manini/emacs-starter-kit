;;; esk/python.el --- setup of python stuff
;;

(require 'python)

(add-hook 'python-mode-hook 'run-coding-hook)

(require 'flymake-python-pyflakes)
(require 'flymake-cursor)

(add-hook 'python-mode-hook 'flymake-python-pyflakes-load)

;; Remove the colon from the electric-indent-chars, it almost always computes the wrong
;; indentation (cfr https://emacs.stackexchange.com/questions/3322/python-auto-indent-problem)
(add-hook 'python-mode-hook
          (lambda ()
            (setq electric-indent-chars (delq ?: electric-indent-chars))))

(define-key python-mode-map (kbd "C-c b") 'python-nav-backward-defun)
(define-key python-mode-map (kbd "C-c f") 'python-nav-forward-defun)
(define-key python-mode-map (kbd "C-c u") 'python-nav-backward-statement)
(define-key python-mode-map (kbd "C-c d") 'python-nav-forward-statement)
