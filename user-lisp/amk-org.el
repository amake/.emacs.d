;;; amk-org.el --- Org-related commands -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Aaron Madlon-Kay

;; Author: Aaron Madlon-Kay
;; Version: 0.1.0
;; URL: https://github.com/amake/.emacs.d
;; Package-Requires: ((emacs "24.4"))

;;; Commentary:

;; Custom org-mode-related commands

;;; Code:

(require 'amk-edit)

(defconst amk-org-markdown-export-buffer-name "*AMK Org Markdown Export*")

;;;###autoload
(defun amk-org-kill-subtree-as-markdown ()
  "Kill current Org subtree as Markdown-formatted text."
  (interactive)
  (require 'ox-md)
  (let ((old-buf (get-buffer amk-org-markdown-export-buffer-name)))
    (when old-buf
      (kill-buffer old-buf)))
  (let ((buf (get-buffer-create  amk-org-markdown-export-buffer-name)))
    (org-export-to-buffer 'md buf
      nil ; async
      t ; subtreep
      nil nil nil (lambda ()
                    (markdown-mode)
                    (mark-whole-buffer)
                    (amk-edit-unfill-paragraph t)
                    (kill-new (buffer-string))))))

(provide 'amk-org)
;;; amk-org.el ends here
