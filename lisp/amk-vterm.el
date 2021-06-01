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

(defun amk-vterm-for-project ()
    "Open vterm for current projectile project."
    (interactive)
    (when-let ((proj-root (projectile-project-root)))
      (let ((curr-vterm (seq-find (lambda (buf)
                                    (with-current-buffer buf
                                      (and
                                       (eq major-mode 'vterm-mode)
                                       (string= default-directory proj-root))))
                                  (buffer-list))))
        (switch-to-buffer
         (or
          curr-vterm
          (let ((current-directory proj-root))
            (vterm)))))))

(provide 'amk-vterm)
;;; amk-vterm.el ends here
