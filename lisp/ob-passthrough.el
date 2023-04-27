;;; ob-passthrough.el --- Passthrough evaluator -*- lexical-binding: t; -*-

;; Author: Matt Curtis
;; Author: Aaron Madlon-Kay
;; Version: 0.0.2
;; URL: https://emacs.stackexchange.com/questions/24247/org-mode-pipe-source-block-output-as-stdin-to-next-source-block/51734#51734
;; Package-Requires: ((emacs "26.1"))

;;; Commentary:

;; This ob evaluates the block as itself, so it can be used as input
;; for another block

;;; Code:

(require 'ob)

(defun org-babel-execute:passthrough (body _params)
  "Pass BODY through verbatim."
  body)

(defun org-babel-execute:$template (body params)
  "Pass BODY through verbatim except for replacing variables defined in PARAMS."
  (let ((var-defs (org-babel--get-vars params)))
    (pcase-dolist (`(,var-name . ,value) var-defs)
      (let ((var (format "$%s" var-name)))
        (setq body (replace-regexp-in-string (regexp-quote var) value body)))))
  body)

;; json output is json
(defalias 'org-babel-execute:json 'org-babel-execute:passthrough)

;; XML output is XML
(defalias 'org-babel-execute:xml 'org-babel-execute:passthrough)

;; CSV output is CSV
(defalias 'org-babel-execute:csv 'org-babel-execute:passthrough)

;; SQL output is SQL
(defalias 'org-babel-execute:sql 'org-babel-execute:$template)

(provide 'ob-passthrough)
;;; ob-passthrough.el ends here
