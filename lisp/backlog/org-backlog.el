;;; org-backlog.el --- Support for links to Backlog issues

;;; Commentary:

;;; Code:

(require 'org)

(defvar backlog-link-template "https://%s.backlog.jp/view/%s")

(org-link-set-parameters "b" :follow #'org-backlog-open)

(defcustom org-backlog-team nil
  "The Backlog team to use when opening links."
  :group 'org-backlog)

(defun org-backlog-open (issue)
  "Open Backlog issue with key ISSUE."
  (browse-url (org-backlog-make-link issue)))

(defun org-backlog-make-link (issue)
  "Create a Backlog URL for issue ISSUE."
  (format backlog-link-template org-backlog-team issue))

;; New-style link shortcut
(add-to-list 'org-link-abbrev-alist
             '("b" . org-backlog-make-link))

(provide 'org-backlog)

;;; org-backlog.el ends here
