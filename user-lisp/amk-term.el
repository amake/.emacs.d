;;; amk-term.el --- Functions for terminal stuff -*- lexical-binding: t; -*-

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

(defun amk-term--buf-default-dir (buf)
  "Get expanded `default-directory' for BUF."
  (expand-file-name (buffer-local-value 'default-directory buf)))

(defun amk-term--project-root ()
  "Get the current project root."
  ;; projectile seems to better handle complex git setups, so prefer it when
  ;; available
  (cond ((featurep 'projectile)
         (projectile-project-root)) ; appears guaranteed to be "expanded"
        ((featurep 'project)
         (let ((project (project-current)))
           (when project
             (expand-file-name (project-root project)))))))

(defun amk-term--for-project (term-pkg)
  "Open TERM-PKG term for current Projectile project or CWD."
  (let* ((proj-root (amk-term--project-root))
         (curr-dir (expand-file-name default-directory))
         (curr-buf (current-buffer))
         (predicate (if proj-root
                        (lambda (buf) (and
                                  (not (eq curr-buf buf))
                                  (string-prefix-p proj-root (amk-term--buf-default-dir buf))))
                      (lambda (buf) (and
                                (not (eq curr-buf buf))
                                (string= curr-dir (amk-term--buf-default-dir buf))))))
         (term-mode (cond ((eq term-pkg 'vterm) 'vterm-mode)
                          ((eq term-pkg 'ghostel) 'ghostel-mode)
                          (t (error "Unknown term-pkg: %s" term-pkg))))
         (term-fn (cond ((eq term-pkg 'vterm) #'vterm)
                 ((eq term-pkg 'ghostel) (lambda () (ghostel t)))
                 (t (error "Unknown term-pkg: %s" term-pkg))))
         (all-terms (seq-filter
                     (lambda (buf)
                       (with-current-buffer buf
                         (and
                          (eq major-mode term-mode)
                          (funcall predicate buf))))
                     (buffer-list)))
         (target-term (if (cadr all-terms) ; at least 2 items
                           (completing-read
                            "Switch to term:"
                            (mapcar (lambda (buf) `(,(buffer-name buf) . ,buf)) all-terms))
                         (car all-terms)))) ; 0 or 1 item
    (switch-to-buffer
     (or target-term
         (let ((default-directory (or proj-root curr-dir)))
           (funcall term-fn))))))

(defun amk-term-vterm-for-project ()
  "Open vterm for current Projectile project or CWD."
  (interactive)
  (amk-term--for-project 'vterm))

(defun amk-term-ghostel-for-project ()
  "Open ghostel term for current Projectile project or CWD."
  (interactive)
  (amk-term--for-project 'ghostel))

(provide 'amk-term)
;;; amk-term.el ends here
