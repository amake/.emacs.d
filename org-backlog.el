;; org-backlog.el - Support for links to Backlog issues

(require 'org)

(org-add-link-type "b" 'org-backlog-open)

(defcustom org-backlog-team nil
  "The Backlog team to use when opening links")

(defun org-backlog-open (issue)
  "Open Backlog issue."
  (browse-url (format "https://%s.backlog.jp/view/%s" org-backlog-team issue)))

(provide 'org-backlog)

;;; org-backlog.el ends here
