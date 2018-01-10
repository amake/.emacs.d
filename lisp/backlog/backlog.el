;;; backlog.el --- Functions for working with Backlog

;;; Commentary:

;;; Code:

(defvar backlog-link-template "https://%s.backlog.jp/view/%s")

(defcustom backlog-team nil
  "The Backlog team to use when opening links."
  :group 'backlog)

(defun browse-backlog-issue (issue)
  "Open Backlog issue with key ISSUE."
  (browse-url (backlog-make-link issue)))

(defun backlog-make-link (issue)
  "Create a Backlog URL for issue ISSUE."
  (format backlog-link-template backlog-team issue))


(provide 'backlog)
;;; backlog.el ends here
