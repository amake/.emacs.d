;;; amk-xml.el --- Custom XML commands -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Aaron Madlon-Kay

;; Author: Aaron Madlon-Kay
;; Version: 0.1.0
;; URL: https://github.com/amake/.emacs.d
;; Package-Requires: ((emacs "26.1"))

;;; Commentary:

;; Custom XML-related commands

;;; Code:

(require 'sgml-mode)

;;;###autoload
(defun amk-xml-pretty-print (beg end)
  "Simple-minded pretty printer for XML, adapted from `sgml-pretty-print'.
Re-indents the code and inserts newlines between BEG and END.
You might want to turn on `auto-fill-mode' to get better results."
  (interactive "r")
  (save-excursion
    (if (< beg end)
	    (goto-char beg)
      (goto-char end)
      (setq end beg)
      (setq beg (point)))
    ;; Don't use narrowing because it screws up auto-indent.
    (setq end (copy-marker end t))
    (with-syntax-table sgml-tag-syntax-table
      (while (re-search-forward "<" end t)
	    (goto-char (match-beginning 0))
        (delete-horizontal-space)
	    (when (looking-back ">" (line-beginning-position))
	      (insert "\n"))
        (forward-char)
	    (sgml-forward-sexp 1))
      (indent-region beg end))))

(provide 'amk-xml)
;;; amk-xml.el ends here
