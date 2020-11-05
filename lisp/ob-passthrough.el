;;; ob-passthrough.el --- Passthrough evaluator -*- lexical-binding: t; -*-

;; Author: Matt Curtis
;; Version: 0.0.1
;; URL: https://emacs.stackexchange.com/questions/24247/org-mode-pipe-source-block-output-as-stdin-to-next-source-block/51734#51734
;; Package-Requires: ((emacs "24.1"))

;;; Commentary:

;; This ob evaluates the block as ifself, so it can be used as input
;; for another block

;;; Code:

(require 'ob)

(defun org-babel-execute:passthrough (body _params)
  "Pass BODY through verbatim."
  body)

;; json output is json
(defalias 'org-babel-execute:json 'org-babel-execute:passthrough)

(provide 'ob-passthrough)
;;; ob-passthrough.el ends here
