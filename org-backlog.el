;; org-backlog.el - Support for links to Backlog issues

(require 'org)

(setq backlog-link-template "https://%s.backlog.jp/view/%s")

(org-add-link-type "b" 'org-backlog-open)

(defcustom org-backlog-team nil
  "The Backlog team to use when opening links")

(defun org-backlog-open (issue)
  "Open Backlog issue."
  (browse-url (org-backlog-make-link issue)))

(defun org-backlog-make-link (issue)
  (format backlog-link-template org-backlog-team issue))

;; New-style link shortcut
(add-to-list 'org-link-abbrev-alist
             '("b" . org-backlog-make-link))

(provide 'org-backlog)

;;; org-backlog.el ends here
