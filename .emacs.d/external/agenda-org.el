;;; Package -- Summary

;;; Commentary:

;;; Code:
(defvar org-custom-daily-agenda
  `(
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
                (org-agenda-overriding-header "Upcoming deadlines (+14d)")))
	(tags-todo "*"
			   ((org-agenda-skip-function '(org-agenda-skip-if nil '(timestamp)))
				(org-agenda-overriding-header "Backlog")))
	))

(provide 'agenda-org)
;;; agenda-org.el ends here
