;;; amk-magit.el --- Custom magit utils -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2020 Aaron Madlon-Kay

;; Author: Aaron Madlon-Kay
;; Version: 0.1.0
;; URL: https://github.com/amake/.emacs.d
;; Package-Requires: ((emacs "25.1") (magit "3.3.0"))

;;; Commentary:

;; Custom magit utils

;;; Code:

(require 'magit)

(defun amk-magit-prune-merged-branches ()
  "Prompt to delete all branches that have been merged into the default branch."
  (interactive)
  (pcase-let* ((`(,remote ,local-head) (magit--get-default-branch))
               (merged-branches (magit-list-merged-branches))
               (to-delete (seq-filter
                           (lambda (branch)
                             (not (string= branch local-head)))
                           merged-branches)))
    (when (yes-or-no-p
           (format "Delete %d branch(es) merged into %s (%s)?"
                   (length to-delete) local-head (mapconcat #'identity to-delete ", ")))
      (magit-branch-delete to-delete))))

(defun amk-magit-reset-default-branch-to-upstream ()
  "Update default branch to latest remote HEAD."
  (interactive)
  (magit-run-git "remote" "update" "--prune")
  (pcase-let* ((`(,remote ,local-head) (magit--get-default-branch))
               (remote-head (concat "refs/remotes/" remote "/" local-head)))
    (magit-branch-reset local-head remote-head)))

(defun amk-magit-show-merged-revision (rev)
  "Show the commit in which REV was merged into the default HEAD branch.

Requires the `when-merged` git command to be installed."
  (interactive
   ;; Defaults from `magit-show-commit'
   (let* ((mcommit (magit-section-value-if 'module-commit))
          (atpoint (or mcommit
                       (magit-thing-at-point 'git-revision t)
                       (magit-branch-or-commit-at-point))))
     (list (or (and (not current-prefix-arg) atpoint)
               (magit-read-branch-or-commit "Show commit" atpoint)))))
  (pcase-let* ((`(,_remote ,merged-to-branch) (magit--get-default-branch))
               (merged-in-commit
                (magit--with-temp-process-buffer
                  (magit-process-git t "when-merged" "-c" rev merged-to-branch)
                  (string-trim (buffer-string)))))
    (apply #'magit-show-commit `(,merged-in-commit ,@(magit-show-commit--arguments)))))

(provide 'amk-magit)
;;; amk-magit.el ends here
