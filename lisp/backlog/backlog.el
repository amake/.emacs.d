;;; backlog.el --- Functions for working with Backlog

;;; Commentary:

;;; Code:

(require 'thingatpt)

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

(defun backlog-issue-p (string)
  "Return t if STRING appears to be a Backlog issue key."
  (if (string-match-p "^[a-z]+-[1-9][0-9]*$" string) t))

(defun browse-backlog-issue-at-point ()
  "Browse the Backlog issue at point, if any."
  (interactive)
  (let ((sym (symbol-name (symbol-at-point))))
    (if (backlog-issue-p sym)
        (browse-backlog-issue sym))))

(provide 'backlog)
;;; backlog.el ends here
