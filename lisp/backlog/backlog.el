;;; backlog.el --- Functions for working with Backlog -*- lexical-binding: t -*-

;;; Commentary:
;; Requires request package (MELPA)

;;; Code:

(require 'thingatpt)
(require 'request)
(require 'cl-lib)

(defconst backlog-link-template "https://%s.backlog.jp/view/%s")
(defconst backlog-api-template "https://%s.backlog.jp/api/v2/%s")

(defcustom backlog-api-key nil
  "Backlog API key to use for API calls."
  :group 'backlog)

(defcustom backlog-team nil
  "The Backlog team to use when opening links."
  :group 'backlog)

;; Backlog REST API functions

(defun backlog-api-url (&optional action)
  "Get the Backlog API endpoint for an ACTION."
  (format backlog-api-template backlog-team (or action "")))

(defun backlog-insert-recent-issue ()
  "Retrieve your own recently viewed issues asynchronously and prompt to insert into buffer."
  (interactive)
  (backlog-recent-issues-async
   nil
   (lambda (issues)
     (let* ((descs (mapcar (lambda (item) (backlog-issue-short-desc (alist-get 'issue item))) issues))
            (selection (completing-read "Issue:" descs)))
       (insert selection)))))

(defun backlog-issue-short-desc (issue)
  "Extract a short description from a raw ISSUE, in the format `KEY SUMMARY`."
  (let ((key (alist-get 'issueKey issue))
        (summary (alist-get 'summary issue)))
    (decode-coding-string (format "%s %s" key summary) 'utf-8)))

(defun backlog-recent-issues-async (user callback)
  "Retrieve USER's recently viewed issues asynchronously and \
process with CALLBACK.  If USER is nil, `myself` is used."
  (let* ((usr (or user "myself"))
         (endpoint (format "users/%s/recentlyViewedIssues" usr)))
    (request
     (backlog-api-url endpoint)
     :params `(("apiKey" . ,backlog-api-key))
     :parser 'json-read
     :success
     (cl-function (lambda (&key data &allow-other-keys)
                    (condition-case err
                        (funcall callback data)
                      (error (message "Callback error: %s" err)))))
     :error
     (cl-function (lambda (&key data &allow-other-keys)
                    (error "Request error: %s" data))))))

;; External browser functions

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
  (let ((sym (thing-at-point 'symbol t)))
    (if (backlog-issue-p sym)
        (browse-backlog-issue sym))))

(provide 'backlog)
;;; backlog.el ends here
