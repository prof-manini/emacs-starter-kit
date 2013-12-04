;;; starter-kit-python.el - setup of python stuff
;;

(require 'python)

(add-hook 'python-mode-hook 'run-coding-hook)

(require 'flymake-python-pyflakes)
(require 'flymake-cursor)

(add-hook 'python-mode-hook 'flymake-python-pyflakes-load)
(setq flymake-python-pyflakes-executable
      (concat esk-top-dir "bin/pyflakes"))
