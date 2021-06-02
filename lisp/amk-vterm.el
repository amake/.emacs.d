;;; amk-vterm.el --- Functions for terminal stuff

;; Copyright (C) 2021 Aaron Madlon-Kay

;; Author: Aaron Madlon-Kay
;; Version: 0.1.0
;; URL: https://github.com/amake/.emacs.d
;; Package-Requires: ((emacs "25.1"))

;;; Commentary:

;; Functions for terminal stuff

;;; Code:

(require 'seq)
(require 'vterm)
(require 'projectile)

(defun amk-vterm--find-vterm-buf (predicate)
  "Find an existing vterm-mode buffer matching PREDICATE."
  (seq-find (lambda (buf)
              (with-current-buffer buf
                (and
                 (eq major-mode 'vterm-mode)
                 (funcall predicate buf))))
            (buffer-list)))

(defun amk-vterm--buf-default-dir (buf)
  "Get expanded `default-directory' for BUF."
  (expand-file-name (buffer-local-value 'default-directory buf)))

(defun amk-vterm-for-project ()
  "Open vterm for current projectile project."
  (interactive)
  (let* ((proj-root (projectile-project-root)) ; appears guaranteed to be "expanded"
         (curr-dir (expand-file-name default-directory))
         (curr-vterm (if proj-root
                         (amk-vterm--find-vterm-buf
                          (lambda (buf) (string-prefix-p proj-root (amk-vterm--buf-default-dir buf))))
                       (amk-vterm--find-vterm-buf
                        (lambda (buf) (string= curr-dir (amk-vterm--buf-default-dir buf)))))))
    (switch-to-buffer
     (or curr-vterm
         (let ((default-directory (or proj-root curr-dir)))
           (vterm))))))

(provide 'amk-vterm)
;;; amk-vterm.el ends here
