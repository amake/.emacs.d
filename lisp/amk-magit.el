;;; amk-magit.el --- Custom magit utils -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2020 Aaron Madlon-Kay

;; Author: Aaron Madlon-Kay
;; Version: 0.1.0
;; URL: https://github.com/amake/.emacs.d
;; Package-Requires: ((emacs "24.4") (magit "3.3.0"))

;;; Commentary:

;; Custom magit utils

;;; Code:

(require 'magit)

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
