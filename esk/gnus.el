;;; esk/gnus.el --- Gnus customization
;;

(eval-when-compile (require 'gnus))

(defun esk-gnus-grace-exit-before-kill-emacs ()
  "Shutdown gnus if active, when exiting emacs."
  (if (and (fboundp 'gnus-alive-p)
           (gnus-alive-p))
      (let ((noninteractive t))
        (gnus-group-exit))))

(defun esk-gnus-switch-to-group-buffer ()
  "Switch to the main Gnus buffer."
  (interactive)
  (switch-to-buffer "*Group*"))

(eval-after-load 'gnus
  '(progn
     (add-hook 'kill-emacs-hook 'esk-gnus-grace-exit-before-kill-emacs)
     (define-key esk-menu-map (kbd "n") (cons "News" 'esk-gnus-switch-to-group-buffer))))
