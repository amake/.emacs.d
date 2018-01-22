;;; org-backlog.el --- Support for links to Backlog issues

;; Package-Requires: (org backlog)

;;; Commentary:

;;; Code:

(require 'org)
(require 'backlog)

(org-link-set-parameters "b" :follow #'browse-backlog-issue)

(provide 'org-backlog)

;;; org-backlog.el ends here
