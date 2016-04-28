;;; lele/notmuch.el --- Notmuch customization
;;

(when (fboundp 'notmuch)
  (eval-when-compile (require 'notmuch nil t))

  (defun esk/notmuch-show-add-deleted-tag ()
    "Tag current message as deleted."
    (interactive)
    (notmuch-show-tag-message "+deleted"))

  (defun esk/notmuch-search-add-deleted-tag ()
    "Tag current message(s) as deleted."
    (interactive)
    (let* ((region (notmuch-search-interactive-region))
           (beg (first region)) (end (second region)))
      (notmuch-search-add-tag '("+deleted") beg end)))

  (defun esk/notmuch-switch-to-hello-buffer ()
    "Switch to the main notmuch buffer."
    (interactive)
    (switch-to-buffer "*notmuch-hello*"))

  (defun esk/notmuch-address-selection-function (prompt addresses first)
    "Use `ido-completing-read' to select one of the addresses."
    (ido-completing-read prompt (cons first addresses)
                         nil nil nil 'notmuch-address-history))

  (eval-after-load 'notmuch
    '(progn
       (define-key notmuch-show-mode-map "d" #'esk/notmuch-show-add-deleted-tag)
       (define-key notmuch-search-mode-map "d" #'esk/notmuch-search-add-deleted-tag)
       (define-key notmuch-common-keymap "g" #'notmuch-refresh-this-buffer)

       (define-key esk/menu-map (kbd "m") (cons "Mail" #'esk/notmuch-switch-to-hello-buffer))

       ;; Do not insert any pointless description of the citation text when replying
       (setq notmuch-mua-reply-insert-header-p-function
             'notmuch-show-reply-insert-header-p-never))))
