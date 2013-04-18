;; starter-kit-python.el - setup of python stuff

(autoload 'virtualenv-activate "virtualenv"
  "Activate a Virtual Environment specified by PATH" t)

(autoload 'virtualenv-workon "virtualenv"
  "Activate a Virtual Environment present using virtualenvwrapper" t)

(require 'python)

(add-hook 'python-mode-hook 'run-coding-hook)

(require 'flymake-python-pyflakes)
(require 'flymake-cursor)

(add-hook 'python-mode-hook 'flymake-python-pyflakes-load)
(setq flymake-python-pyflakes-executable
      (concat esk-dotfiles-dir "bin/pyflakes"))

(provide 'starter-kit-python)
