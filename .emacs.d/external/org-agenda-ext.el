;; Package -- Summary

;;; Commentary:

;;; Code:
(require 'org-capture)

(defgroup vincnt054 nil
  "Quick manipulation of textual checkboxes."
  :group 'org-custom)

(defun org-custom-agenda()
	(interactive)
	(org-agenda nil "A"))

;; (defmacro vincnt054/org-find-agenda ()
;;   (lambda ()
;; 	 (interactive)
;; 	 (let ((fpath (concat org-directory "/"
;; 						  (read-answer "File: "
;; 									   '(("deanima" ?d "for my own soul")
;; 										 ("proletarii" ?p "for my line of work")
;; 										 ("domus" ?f "for my girlfriend and family")
;; 										 ("inbox" ?i "for the unknown")))
;; 						  ".org")))
;; 	   (set-buffer (org-capture-target-buffer fpath)))))

(defcustom vincnt054/org-custom-daily-agenda
  `((tags-todo "*" ((org-agenda-skip-function '(org-agenda-skip-if nil '(timestamp)))
					(org-agenda-overriding-header "Backlog")))
	(agenda "" ((org-agenda-span 1)
				(org-deadline-warning-days 0)
				(org-scheduled-past-days 0)
				(org-agenda-day-face-function (lambda (date) 'org-agenda-date))
				(org-agenda-format-date "%A %-e %B %Y")
				(org-agenda-overriding-header "Today's agenda")))
	(agenda "" ((org-agenda-start-on-weekday nil)
				(org-agenda-start-day "+1d")
				(org-agenda-span 3)
				(org-deadline-warning-days 0)
				(org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
				(org-agenda-overriding-header "Upcoming (+3d)")))
	(agenda "" ((org-agenda-time-grid nil)
				(org-agenda-start-on-weekday nil)
				(org-agenda-start-day "+4d")
				(org-agenda-span 14)
				(org-deadline-warning-days 0)
				(org-agenda-entry-types '(:deadline))
				(org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
				(org-agenda-overriding-header "Upcoming deadlines (+14d)"))))
  "Org daily agenda setting."
  :group 'vincnt054)

(defcustom vincnt054/org-custom-task
  `(("t" "Task" entry
	   ;; (function ,vincnt054/org-find-agenda)
	   (function (lambda ()
				   (interactive)
				   (let ((fpath (concat org-directory "/"
										(read-answer "File: "
													 '(("deanima" ?d "for my own soul")
													   ("proletarii" ?p "for my line of work")
													   ("domus" ?f "for my girlfriend and family")
													   ("inbox" ?i "for the unknown")))
										".org")))
					 (set-buffer (org-capture-target-buffer fpath)))))
	   "* TODO %%?\n:PROPERTIES:\n:CAPTURED: %%U\n:END:"
	   :empty-lines-before 1)
	  ("r" "Report" entry
	   ;; (function ,vincnt054/org-find-agenda)
	   (function (lambda ()
				   (interactive)
				   (let ((fpath (concat org-directory "/"
										(read-answer "File: "
													 '(("deanima" ?d "for my own soul")
													   ("proletarii" ?p "for my line of work")
													   ("domus" ?f "for my girlfriend and family")
													   ("inbox" ?i "for the unknown")))
										".org")))
					 (set-buffer (org-capture-target-buffer fpath)))))
	   "* REPORTED %%?\n:PROPERTIES:\n:CAPTURED: %%U\n:END:"
	   :empty-lines-before 1)
	  ("b" "Bug" entry
	   ;; (function ,vincnt054/org-find-agenda)
	   (function (lambda ()
				   (interactive)
				   (let ((fpath (concat org-directory "/"
										(read-answer "File: "
													 '(("deanima" ?d "for my own soul")
													   ("proletarii" ?p "for my line of work")
													   ("domus" ?f "for my girlfriend and family")
													   ("inbox" ?i "for the unknown")))
										".org")))
					 (set-buffer (org-capture-target-buffer fpath)))))
	   "* BUG %%?\n:PROPERTIES:\n:CAPTURED: %%U\n:END:"
	   :empty-lines-before 1))
  "Org capture task."
  :group 'vincnt054)

(message "%s" vincnt054/org-custom-custom-task)
(provide 'org-agenda-ext)
;;; org-agenda-ext.el ends here
