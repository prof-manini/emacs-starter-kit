;;; esk/completion.el --- A few common completion tricks
;;

(require 'company)
(require 'company-jedi)

(add-to-list 'company-backends 'company-jedi)

(global-company-mode)

;; ignore some more directory patterns
(add-to-list 'completion-ignored-extensions ".egg-info/")
(add-to-list 'completion-ignored-extensions "__pycache__/")
