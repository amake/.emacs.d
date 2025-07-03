;;; amk-tabulated-list.el --- Utils for tabulated lists -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Aaron Madlon-Kay

;; Author: Aaron Madlon-Kay
;; Version: 0.1.0
;; URL: https://github.com/amake/.emacs.d
;; Package-Requires: ((emacs "24.3"))

;;; Commentary:

;; Utils for tabulated lists

;;; Code:

(defun amk-tabulated-list--get-current-column-nb ()
  "Return the current column number in a tabulated list."
  (let ((start (current-column))
        (entry (tabulated-list-get-entry))
        (nb-cols (length tabulated-list-format))
        (col-nb 0)
        (total-width 0)
        (found nil))
    (while (and (not found)
                (< col-nb nb-cols))
      (if (>= start
              (setq total-width
                    (+ total-width
                       (max (cadr (aref tabulated-list-format
                                        col-nb))
                            (let ((desc (aref entry col-nb)))
                              (string-width (if (stringp desc)
                                                desc
                                              (car desc)))))
                       (or (plist-get (nthcdr 3 (aref tabulated-list-format
                                                      col-nb))
                                      :pad-right)
                           1))))
          (setq col-nb (1+ col-nb))
        (setq found t)))
    col-nb))

(defun amk-tabulated-list--get-column-max-width (col-nb)
  "Return the maximum width of column COL-NB in the current tabulated list."
  (let* ((name (car (aref tabulated-list-format col-nb)))
         (max-width (string-width name)))
    (dolist (entry tabulated-list-entries)
      (let ((desc (aref (cadr entry) col-nb)))
        (setq max-width
              (max max-width
                   (string-width (if (stringp desc) desc (car desc)))))))
    max-width))

(defun amk-tabulated-list-fit-current-column-width (&optional shrink)
  "Fit the current column in a tabulated list to its maximum width.

If SHRINK is non-nil, shrink the column to its maximum width if it is larger
than the current width."
  (interactive "P")
  (let* ((col-nb (amk-tabulated-list--get-current-column-nb))
         (current-width (cadr (aref tabulated-list-format col-nb)))
         (max-width (amk-tabulated-list--get-column-max-width col-nb)))
    (when (> max-width (if shrink 0 current-width))
      (setq tabulated-list-format (copy-tree tabulated-list-format t))
      (setf (cadr (aref tabulated-list-format col-nb))
            max-width)
      (tabulated-list-print t)
      (tabulated-list-init-header))))

(provide 'amk-tabulated-list)
;;; amk-tabulated-list.el ends here
