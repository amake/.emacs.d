;;; amk-browse.el --- Custom browsing commands

;; Package-Requires: (thingatpt cl-lib)

;;; Commentary:

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

(defun amk-multibrowse ()
  "Browse the symbol at point."
  (interactive)
  (let ((sym (thing-at-point 'symbol t)))
    (if sym (amk-multibrowse-symbol sym))))

(defun amk-multibrowse-symbol (symbol)
  "Multibrowse the given SYMBOL."
  (unless (cl-loop for (test . action) in amk-browse-alist
                   if (and (funcall test symbol) (funcall action symbol))
                   return t)
    (funcall amk-browse-fallback-action)))

(provide 'amk-browse)
;;; amk-browse.el ends here
