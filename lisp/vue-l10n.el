;;; vue-l10n.el --- Tools for Vue L10N -*- lexical-binding: t -*-

;; Copyright (C) 2022 Aaron Madlon-Kay

;; Author: Aaron Madlon-Kay
;; Version: 0.1.0
;; URL: https://github.com/amake/.emacs.d
;; Package-Requires: ((emacs "27.1"))
;; Keywords: languages

;; This file is not part of GNU Emacs.

;; vue-l10n.el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3, or (at your option) any later version.
;;
;; vue-l10n.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.
;;
;; You should have received a copy of the GNU General Public License along with
;; vue-l10n.el.  If not, see http://www.gnu.org/licenses.

;;; Commentary:

;; vue-l10n.el is a package providing helpful functions for localizing Vue.js
;; applications

;;; Code:

(eval-when-compile (require 'subr-x))
(require 'thingatpt)



;;; Public variables

(defvar-local vue-l10n-source-lang "en")

(put 'vue-l10n-source-lang 'safe-local-variable #'stringp)

(defvar-local vue-l10n-localizable-attributes '("label" "placeholder"))

(defun vue-l10n--localizable-attributes-list-p (val)
  "Validate that VAL is a list of localizable attributes."
  (and (listp val)
       (seq-every-p #'stringp val)))

(put 'vue-l10n-localizable-attributes 'safe-local-variable #'vue-l10n--localizable-attributes-list-p)


;;; Code generation

(defconst vue-l10n--ref-text-templ "{{ $t('%s') }}")

(defconst vue-l10n--ref-attr-templ "$t('%s')")

(defun vue-l10n--gen-string-ref (id type)
  "Generate a reference for TYPE to the string with ID."
  (cond ((eq type 'vue-text-string) (format vue-l10n--ref-text-templ id))
        ((eq type 'vue-attr-string) (format vue-l10n--ref-attr-templ id))
        (t (error "Unknown type: %s" type))))

(defconst vue-l10n--def-templ
  "\"%s\": \"%s\",\n")

(defun vue-l10n--gen-string-def (id value)
  "Generate a l10n string definition with ID and VALUE."
  (format vue-l10n--def-templ id value))

(defconst vue-l10n--comment-templ "<!-- %s -->")

(defun vue-l10n--gen-comment (contents)
  "Generate a comment with CONTENTS."
  (format vue-l10n--comment-templ contents))

(defconst vue-l10n--i18n-block-templ
  "\n<i18n lang=\"json\">\n{\n  \"%s\": {\n  }\n}\n</i18n>\n")

(defun vue-l10n--gen-i18n-block ()
  "Generate an <i18n> block with an entry for the source language."
  (format vue-l10n--i18n-block-templ vue-l10n-source-lang))


;;; Internal utilities

(defun vue-l10n--forward-vue-text-string (&optional arg)
  "Move to the end or beginning of the string at point.
Go forward for positive ARG, or backward for negative ARG.
Assumes start in middle of string.  Not meant for general use;
only for making `bounds-of-thing-at-point' work."
  (interactive "^p")
  (if (natnump arg)
      (progn
        (re-search-forward "[^<]+<" nil 'move)
        (backward-char))
    (re-search-backward ">[^>]" nil 'move)
    (forward-char)))

(put 'vue-text-string 'forward-op #'vue-l10n--forward-vue-text-string)

(defun vue-l10n--forward-vue-attr-string (&optional arg)
  "Move to the end or beginning of the string at point.
Go forward for positive ARG, or backward for negative ARG.
Assumes start in middle of string.  Not meant for general use;
only for making `bounds-of-thing-at-point' work."
  (interactive "^p")
  (if (natnump arg)
      (progn
        (re-search-forward "[^\"]+\"" nil 'move)
        (backward-char))
    (re-search-backward "=\"[^>]" nil 'move)
    (forward-char 2)))

(put 'vue-attr-string 'forward-op #'vue-l10n--forward-vue-attr-string)

(defun vue-l10n--normalize-string (string)
  "Normalize a Vue.js STRING."
  (string-trim string))

(defun vue-l10n--i18n-block-exists-p ()
  "Check to see if an <i18n> block exists in the current file."
  (save-excursion
    (goto-char 1)
    (re-search-forward "^[ \t]*<i18n" nil t)))

(defun vue-l10n--ensure-i18n-block ()
  "Ensure that an <i18n> block exists in the current file."
  (save-excursion
    (goto-char 1)
    (unless (re-search-forward "^[ \t]*<i18n" nil t)
      (goto-char (max-char))
      (insert (vue-l10n--gen-i18n-block)))))

(defconst vue-l10n--i18n-block-pattern
  "^[ \t]*<i18n[^>]*>\n?\\(\\(?:.\\|\n\\)*\\)\n?</i18n>")

(defun vue-l10n--append-to-i18n-block (lang key value)
  "Append KEY and VALUE to the LANG object in the <i18n> block."
  (vue-l10n--ensure-i18n-block)
  (let ((data (vue-l10n--read-i18n-block)))
    (puthash key value (gethash lang data))
    (save-excursion
      (goto-char 1)
      (re-search-forward vue-l10n--i18n-block-pattern)
      (let ((beg (match-beginning 1))
            (end (match-end 1))
            (temp-buffer (generate-new-buffer " *temp*" t)))
        (unwind-protect
            (progn
              (narrow-to-region beg end)
              (with-current-buffer temp-buffer
                (insert (json-serialize data) "\n")
                (json-pretty-print-buffer))
              (replace-buffer-contents temp-buffer))
          (kill-buffer temp-buffer)
          (widen))))))

(defun vue-l10n--read-i18n-block ()
  "Read contents of <i18n> block as a hash table."
  (when (vue-l10n--i18n-block-exists-p)
    (save-excursion
      (goto-char 1)
      (re-search-forward vue-l10n--i18n-block-pattern)
      (let ((json (match-string 1)))
        (json-parse-string json)))))

(defun vue-l10n--get-existing-ids ()
  "Return a hash table of existing string IDs from the <i18n> block."
  (if (vue-l10n--i18n-block-exists-p)
      (let* ((data (vue-l10n--read-i18n-block)))
        (gethash vue-l10n-source-lang data))
    (make-hash-table :test #'equal)))

(defun vue-l10n--read-id (existing)
  "Prompt user for a string ID, optionally choosing from EXISTING."
  (let ((response (completing-read "String ID [skip]: "
                                   existing
                                   nil ; predicate
                                   nil ; require-match
                                   nil ; initial-input
                                   nil ; hist
                                   "" ; def
                                   )))
    (if (string-empty-p response)
        nil
      response)))

(defun vue-l10n--find-template-bounds ()
  "Find the bounds of the root <template> block."
  (save-excursion
    (let (beg end)
      (goto-char 1)
      (re-search-forward "^[ \t]*<template>")
      (setq beg (match-end 0))
      (goto-char (max-char))
      (re-search-backward "^[ \t]*</template>")
      (setq end (match-beginning 0))
      `(,beg . ,end))))

(defun vue-l10n--interpolation-p (str)
  "Return non-nil if STR is a Vue interpolation like {{this}}."
  (string-match-p "^{{.*}}$" str))

(defun vue-l10n--build-string-search-pattern ()
  "Build a pattern for searching for localizable strings.

Match 1: `vue-text-string'
Match 2: `vue-attr-string'"
  (mapconcat
   #'identity
   `(">\\([^<]+\\)<"
     ,(format "\\(?:%s\\)=\"\\([^\"]+\\)\""
              (mapconcat #'identity vue-l10n-localizable-attributes "\\|")))
   "\\|"))


;;; Public interface

;;;###autoload
(defun vue-l10n-externalize-at-point ()
  "Replace a string with a lookup.

The corresponding string definition will be put on the kill ring for yanking
into the <i18n> block."
  (interactive)
  (let* ((type (cond ((bounds-of-thing-at-point 'vue-text-string) 'vue-text-string)
                     ((bounds-of-thing-at-point 'vue-attr-string) 'vue-attr-string)
                     (t (error "No Vue string at point!"))))
         (bounds (bounds-of-thing-at-point type))
         (beg (car bounds))
         (end (cdr bounds))
         (value (vue-l10n--normalize-string
                 (buffer-substring beg end)))
         (existing (vue-l10n--get-existing-ids))
         (id (vue-l10n--read-id existing))
         (definition (vue-l10n--gen-string-def id value))
         (reference (vue-l10n--gen-string-ref id type))
         (comment (vue-l10n--gen-comment value)))
    (when id ; null id means user chose to skip
      (vue-l10n--ensure-i18n-block)
      (delete-region beg end)
      (goto-char beg) ; In case e.g. user moved point while choosing the id
      (insert reference)
      ;; (insert comment)
      (unless (gethash id existing)
        (kill-new definition)))))

;;;###autoload
(defun vue-l10n-externalize-all ()
  "Interactively externalize all string literals in the buffer.

The corresponding string definitions will be appended to the
source (`vue-l10n-source-lang') section of the <i18n> block."
  (interactive)
  (let (history
        (existing (vue-l10n--get-existing-ids))
        (template-end (cdr (vue-l10n--find-template-bounds)))
        (search-pattern (vue-l10n--build-string-search-pattern)))
    (unwind-protect
        (while (re-search-forward search-pattern template-end t)
          ;; Store match bounds now so they don't get clobbered
          (let* ((group (cond ((match-string 1) 1)
                              ((match-string 2) 2)))
                 (type (cond ((= group 1) 'vue-text-string)
                             ((= group 2) 'vue-attr-string)))
                 (beg (match-beginning group))
                 (end (match-end group))
                 (value (vue-l10n--normalize-string
                         (match-string group)))) ; Empty match
            (unless (or (string-empty-p value)
                        (vue-l10n--interpolation-p value))
              (goto-char end) ; Ensure point is at end of string, not whole match
              (push-mark beg)
              (activate-mark)
              (let* ((id (vue-l10n--read-id existing))
                     (reference (vue-l10n--gen-string-ref id type))
                     (comment (vue-l10n--gen-comment value)))
                (when id ; null id means user chose to skip
                  ;; `replace-match' sometimes fails with
                  ;; "Match data clobbered by buffer modification hooks"
                  ;; so delete and insert instead. Previously:
                  ;;(replace-match reference t t)
                  (delete-region beg end)
                  (goto-char beg) ; In case e.g. user moved point while choosing the id
                  (insert reference)
                  ;; (insert comment)
                  (unless (or (member id history)
                              (gethash id existing))
                    (vue-l10n--append-to-i18n-block vue-l10n-source-lang id value)
                    (setq template-end (cdr (vue-l10n--find-template-bounds))))
                  (push id history)
                  (puthash id t existing))))))
      (deactivate-mark))))

(provide 'vue-l10n)
;;; vue-l10n.el ends here
