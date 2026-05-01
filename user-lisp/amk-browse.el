;;; amk-browse.el --- Custom browsing commands -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2020 Aaron Madlon-Kay

;; Author: Aaron Madlon-Kay
;; Version: 0.1.0
;; URL: https://github.com/amake/.emacs.d
;; Package-Requires: ((emacs "24.3"))

;; Package-Requires: (thingatpt cl-lib)

;;; Commentary:

;; Multi-browse dispatch functionality

;;; Code:

(require 'cl-lib)
(require 'thingatpt)

(defcustom amk-browse-alist nil
  "An alist of methods to try when browsing a token.

Each item is a cons cell, where the car is a predicate to test
the browse token against, and the cdr is the browsing action to
be called against the function if the predicate returns t."
  :group 'amk-browse
  :type '(string))

(defcustom amk-browse-fallback-action #'browse-url-at-point
  "The fallback browse action to perform if no predicates in \
`amk-browse-alist` match."
  :group 'amk-browse
  :type '(function))

(defun amk-browse-multi ()
  "Browse the symbol at point."
  (interactive)
  (let ((sym (thing-at-point 'symbol t)))
    (if sym (amk-browse-symbol sym))))

(defun amk-browse-symbol (symbol)
  "Multibrowse the given SYMBOL."
  (unless (cl-loop for (test . action) in amk-browse-alist
                   if (and (funcall test symbol) (funcall action symbol))
                   return t)
    (funcall amk-browse-fallback-action)))

(provide 'amk-browse)
;;; amk-browse.el ends here
