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

(defun amk-magit-default-head-branch ()
  "Return the default HEAD branch for the current repository."
  (magit--with-temp-process-buffer
    (magit-process-git t "symbolic-ref" (concat "refs/remotes/" (magit-get-current-remote) "/HEAD"))
    (string-trim (buffer-string))))

(defun amk-magit-reset-default-branch-to-upstream ()
  "Update default branch to latest remote HEAD."
  (interactive)
  (magit-run-git "remote" "update" "--prune")
  (let* ((remote (magit-get-current-remote))
         (remote-head (amk-magit-default-head-branch))
         (local-head (substring-no-properties remote-head
                                              (length (concat "refs/remotes/" remote "/")))))
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
  (let* ((merged-to-branch (amk-magit-default-head-branch))
         (merged-in-commit
          (magit--with-temp-process-buffer
            (magit-process-git t "when-merged" "-c" rev merged-to-branch)
            (string-trim (buffer-string)))))
    (magit-show-commit merged-in-commit (magit-show-commit--arguments))))

(provide 'amk-magit)
;;; amk-magit.el ends here
