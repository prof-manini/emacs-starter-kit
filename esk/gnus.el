;;; esk/gnus.el --- Gnus customization
;;

(eval-when-compile
  (require 'gnus)
  (require 'gnus-group))

(defun esk/gnus-grace-exit-before-kill-emacs ()
  "Shutdown gnus if active, when exiting emacs."
  (if (and (fboundp 'gnus-alive-p)
           (gnus-alive-p))
      (let ((noninteractive t))
        (gnus-group-exit))))

(defun esk/gnus-switch-to-group-buffer ()
  "Switch to the main Gnus buffer."
  (interactive)
  (switch-to-buffer "*Group*"))

; Store all Gnus staff under user's subdirectory, not tracked by darcs
(csetq gnus-home-directory (concat esk/user-specific-dir "gnus/"))

(eval-after-load 'gnus
  '(progn
     (add-hook 'kill-emacs-hook #'esk/gnus-grace-exit-before-kill-emacs)
     (define-key esk/menu-map (kbd "n") (cons "News" #'esk/gnus-switch-to-group-buffer))))
