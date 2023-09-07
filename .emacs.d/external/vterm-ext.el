;;; Package -- Summary

;;; Commentary:

;;; Code:
(declare-function vterm-toggle-insert-cd "vterm")

(defun vterm-toggle-cd-buffer()
  (interactive)
  (let ((inhibit-message t))
	(vterm-toggle-insert-cd)
	(vterm-toggle-insert-cd)))

(provide 'vterm-ext)
;;; vterm-ext.el ends here
