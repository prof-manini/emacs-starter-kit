;;; esk/completion.el --- A few common completion tricks
;;

(require 'company)

;; Ignore some more directory patterns
(add-to-list 'completion-ignored-extensions ".egg-info/")
(add-to-list 'completion-ignored-extensions "__pycache__/")

;; Respect the case, I can't imagine why these aren't the default
(setq company-dabbrev-downcase nil)
(setq company-dabbrev-ignore-case t)

(global-company-mode)
