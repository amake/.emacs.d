;;; amk-vterm.el --- Functions for terminal stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Aaron Madlon-Kay

;; Author: Aaron Madlon-Kay
;; Version: 0.1.0
;; URL: https://github.com/amake/.emacs.d
;; Package-Requires: ((emacs "25.1"))

;;; Commentary:

;; Functions for terminal stuff

;;; Code:

(require 'seq)
(require 'projectile nil t)
(require 'project nil t)

(defun amk-vterm--find-vterm-bufs (predicate)
  "Find existing vterm-mode buffers matching PREDICATE."
  (seq-filter (lambda (buf)
                (with-current-buffer buf
                  (and
                   (eq major-mode 'vterm-mode)
                   (funcall predicate buf))))
              (buffer-list)))

(defun amk-vterm--buf-default-dir (buf)
  "Get expanded `default-directory' for BUF."
  (expand-file-name (buffer-local-value 'default-directory buf)))

(defun amk-vterm--project-root ()
  "Get the current project root."
  ;; projectile seems to better handle complex git setups, so prefer it when
  ;; available
  (cond ((featurep 'projectile)
         (projectile-project-root)) ; appears guaranteed to be "expanded"
        ((featurep 'project)
         (let ((project (project-current)))
           (when project
             (expand-file-name (project-root project)))))))

(defun amk-vterm-for-project ()
  "Open vterm for current Projectile project or CWD."
  (interactive)
  (let* ((proj-root (amk-vterm--project-root))
         (curr-dir (expand-file-name default-directory))
         (curr-buf (current-buffer))
         (predicate (if proj-root
                        (lambda (buf) (and
                                  (not (eq curr-buf buf))
                                  (string-prefix-p proj-root (amk-vterm--buf-default-dir buf))))
                      (lambda (buf) (and
                                (not (eq curr-buf buf))
                                (string= curr-dir (amk-vterm--buf-default-dir buf))))))
         (all-vterms (amk-vterm--find-vterm-bufs predicate))
         (target-vterm (if (cadr all-vterms) ; at least 2 items
                           (completing-read
                            "Switch to vterm:"
                            (mapcar (lambda (buf) `(,(buffer-name buf) . ,buf)) all-vterms))
                         (car all-vterms)))) ; 0 or 1 item
    (switch-to-buffer
     (or target-vterm
         (let ((default-directory (or proj-root curr-dir)))
           (vterm))))))

(provide 'amk-vterm)
;;; amk-vterm.el ends here
