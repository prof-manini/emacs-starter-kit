;;; esk/gnus.el --- Gnus customization
;;

(eval-when-compile
  (require 'gnus)
  (require 'gnus-group)
  (require 'gnus-start)
  (require 'gnus-art)
  (require 'gnus-msg)
  (require 'mm-decode))

;; Since I only use Gnus to read the newsgroups, disable reading and writing
;; the generic newsrc file
(csetq gnus-read-newsrc-file nil)
(csetq gnus-save-newsrc-file nil)

; Store all Gnus staff under a directory ignored by git
(csetq gnus-home-directory (concat esk/top-dir "gnus/"))
(csetq gnus-init-file (concat gnus-home-directory "gnus-init"))
(csetq gnus-startup-file (concat gnus-home-directory "newsrc"))

;; Also, I manually select which newsgroups I'm gonna follow, so don't bother
;; with keeping a notion of "new" newsgroups
(csetq gnus-save-killed-list nil)
(csetq gnus-check-new-newsgroups nil)

;; Customize appearence
(csetq gnus-permanently-visible-groups ".")
(csetq gnus-summary-line-format "%U%R%z%d %(%[%-20,20a%]%) %I%s\n")
(csetq gnus-summary-same-subject "⤷")

;; Prefer plain text alternative
(csetq mm-discouraged-alternatives '("text/html" "text/richtext" "image/.*")
       mm-automatic-display (remove "text/html" mm-automatic-display)
       gnus-buttonized-mime-types '("multipart/alternative" "multipart/signed"))

;; Archive - putting sent mail and news somewhere
(csetq gnus-message-archive-group
       '((if (message-news-p)
             (concat "sent-news-"
                     (format-time-string "%b-%y"))
           (concat "sent-mail-"
                   (format-time-string "%b-%y")))))

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

(defun esk/gnus-subscribe-to-user-groups ()
  "Subscribe to user's groups, if defined.
If user wants, she should define the list esk/gnus-user-groups in her
`~/.emacs.d/her-name/gnus.el' containing the newsgroups she's interested to."
  (require 'gnus-start)
  (when (boundp 'esk/gnus-user-groups)
    (mapc #'gnus-subscribe-newsgroup (reverse esk/gnus-user-groups))))

(eval-after-load 'gnus
  '(progn
     (esk/gnus-subscribe-to-user-groups)
     (add-hook 'kill-emacs-hook #'esk/gnus-grace-exit-before-kill-emacs)
     (define-key esk/menu-map (kbd "n") (cons "News" #'esk/gnus-switch-to-group-buffer))))
