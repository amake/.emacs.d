;;; edit-string.el --- Indirectly edit string literals  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Aaron Madlon-Kay

;; Author: Aaron Madlon-Kay
;; Version: 0.1.0
;; URL: https://github.com/amake/.emacs.d
;; Package-Requires: ((emacs "28.1") (edit-indirect "0.1.10"))

;;; Commentary:

;; Edit string literals indirectly, with smart indentation handling.

;;; Code:

(require 'thingatpt)
(require 'edit-indirect)

;; Trimming logic

(defun edit-string--trim-leading-blank-lines ()
  "Trim leading blank lines from the current buffer."
  (save-excursion
    (goto-char 0)
    (when (re-search-forward "[^ \t\n\r]" nil t)
      (goto-char (line-beginning-position))
      (let ((leading (buffer-substring 1 (point))))
        (delete-region 1 (point))
        leading))))

(defun edit-string--trim-trailing-blank-lines ()
  "Trim trailing blank lines from the current buffer."
  (save-excursion
    (goto-char (point-max))
    (when (re-search-backward "[^ \t\n\r]" nil t)
      (goto-char (+ (point) 2))
      (let ((trailing (buffer-substring (point) (point-max))))
        (delete-region (point) (point-max))
        trailing))))

(defun edit-string--trim-common-indent ()
  "Trim and return the longest common indent string in the current buffer."
  (let ((indent (edit-string--compute-common-indent)))
    (save-excursion
      (goto-char 0)
      (while (not (eobp))
        (delete-region (point) (+ (point) (length indent)))
        (forward-line)))
    indent))

(defun edit-string--trim ()
  "Trim leading & trailing whitespace and common indent from current buffer.

Returns a function that will restore the trimmed whitespace."
  (let ((leading (edit-string--trim-leading-blank-lines))
        (trailing (edit-string--trim-trailing-blank-lines))
        (indent (edit-string--trim-common-indent)))
    (lambda () (edit-string--restore-trimmed leading trailing indent))))

(defun edit-string--restore-trimmed (leading trailing indent)
  "Restore trimmed LEADING, TRAILING, and INDENT strings."
  (save-excursion
    (goto-char 0)
    (insert leading)
    (while (not (eobp))
      (goto-char (line-beginning-position))
      (insert indent)
      (forward-line))
    (insert trailing)))

;; Util

(defun edit-string--common-prefix (s1 s2)
  "Compute the common prefix of S1 and S2."
  (let ((cmp (compare-strings s1 0 (length s1) s2 0 (length s2))))
    (if (eq cmp t)
        s1
      (substring s1 0 (1- (abs cmp))))))

(defun edit-string--collect-indents ()
  "Return a hash of all indent strings from the current buffer."
  (save-excursion
    (goto-char 0)
    (let ((indents (make-hash-table :test #'equal)))
      (condition-case nil
          (while (and (re-search-forward "^[ \t]*" nil t) (not (eobp)))
            (let ((indent (match-string-no-properties 0)))
              (puthash indent t indents)
              ;; An empty-string indent means there is no common indent
              ;; to remove, so we can exit early.
              (when (= 0 (length indent))
                (signal 'quit nil))))
        (quit nil))
      indents)))

(defun edit-string--compute-common-indent ()
  "Compute the longest common indent string in the current buffer."
  (let ((indents (edit-string--collect-indents)))
    (if (gethash "" indents)
        ""
      (let ((keys (hash-table-keys indents)))
        (seq-reduce #'edit-string--common-prefix keys (car keys))))))

;; Advice for edit-indirect. Could be removed if upstreamed; see
;; https://github.com/Fanael/edit-indirect/issues/6

(defvar edit-string-after-save-hook nil
  "Hook run after saving an edit-indirect buffer.")

(defun edit-string--after-save-advice ()
  "Run `edit-string-after-save-hook'."
  (run-hooks 'edit-string-after-save-hook))

(advice-add #'edit-indirect-save :after #'edit-string--after-save-advice)

;; Public

(defvar edit-string-guess-mode-alist nil
  "Alist for setting the major mode of an edit-indirect buffer.

Content is (PREDICATE . FUNC).")

(defun edit-string--guess-mode ()
  "Guess mode a bit more smartly than `edit-indirect-default-guess-mode'."
  (condition-case nil
      (progn
        (pcase-dolist (`(,pred . ,func) edit-string-guess-mode-alist)
         (when (save-excursion (goto-char 0) (funcall pred))
           (funcall func)
           (signal 'quit nil)))
        (normal-mode))
    (quit nil)))

(defun edit-string-at-point ()
  "Indirectly edit string at point."
  (interactive)
  (pcase-let ((`(,beg . ,end) (bounds-of-thing-at-point 'string)))
    (with-current-buffer (edit-indirect-region (1+ beg) (1- end) t)
      (edit-string--guess-mode)
      (let ((restore-func (edit-string--trim)))
        (add-hook 'edit-indirect-before-commit-hook restore-func nil t)
        (add-hook 'edit-string-after-save-hook #'edit-string--trim nil t)))))

(provide 'edit-string)
;;; edit-string.el ends here
