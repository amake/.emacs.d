;;; flycheck-languagetool.el --- Natural language checker -*- lexical-binding: t -*-

;; Check natural language with LanguageTool; see https://languagetool.org/

;;; Commentary:

;; Check natural language with LanguageTool.

;;; Code:

(require 'flycheck)

(defvar-local flycheck-languagetool-language nil
  "The language of the buffer contents.")

(put 'flycheck-languagetool-language 'safe-local-variable #'stringp)

(defvar-local flycheck-languagetool-line-by-line nil
  "Whether to check the buffer line-by-line.")

(put 'flycheck-languagetool-line-by-line 'safe-local-variable #'booleanp)

(defcustom flycheck-languagetool-mother-tongue nil
  "The author's mother tongue, to identify false friends."
  :group 'flycheck-languagetool
  :type 'string)

(put 'flycheck-languagetool-mother-tongue 'safe-local-variable #'stringp)

(defcustom flycheck-languagetool-disable-rules-alist nil
  "A list of LanguageTool rules to disable.  Format is an alist
of (MAJOR-MODE . (STRING ...)).  A MAJOR-MODE of t is used as a
catch-all."
  :group 'flycheck-languagetool
  :type '(alist :key-type symbol :value-type (list string)))

(put 'flycheck-languagetool-disable-rules-alist 'safe-local-variable
     #'flycheck-languagetool--disable-rules-alist-valid-p)

(defun flycheck-languagetool--disable-rules-alist-valid-p (alist)
  "Check that ALIST is a valid list of disable rules."
  (seq-every-p (lambda (item)
                 (and (symbolp (car item))
                      (seq-every-p #'stringp (cdr item))))
               alist))

(flycheck-define-checker languagetool
  "A LanguageTool natural language checker.

See URL `https://languagetool.org/'."
  :command ("languagetool"
            (eval (if flycheck-languagetool-language
                      `("-l" ,flycheck-languagetool-language)
                    "-adl"))
            (eval (when flycheck-languagetool-mother-tongue
                    `("-m" ,flycheck-languagetool-mother-tongue)))
            (eval (when flycheck-languagetool-line-by-line
                    "--line-by-line"))
            (eval (let ((disabled (flycheck-languagetool-get-disabled-rules)))
                    (when disabled
                      `("-d" ,(mapconcat #'identity disabled ",")))))
            "-")
  :standard-input t
  ;; Modes taken from flycheck-textlint-config
  :modes (text-mode markdown-mode gfm-mode message-mode adoc-mode
                    mhtml-mode latex-mode org-mode rst-mode)
  :error-patterns
  ((warning line-start
            (one-or-more digit) ".) Line " line ", column " column
            ", Rule ID: " (id (one-or-more any)) "\n"
            "Message: " (message)
            line-end)))

(defun flycheck-languagetool-get-disabled-rules ()
  "Compute the disabled rules for the current mode."
  (or (alist-get major-mode flycheck-languagetool-disable-rules-alist)
      (alist-get t flycheck-languagetool-disable-rules-alist)))

(defun flycheck-languagetool-setup ()
  "Set up the flycheck-languagetool checker."
  (interactive)
  (add-to-list 'flycheck-checkers 'languagetool))

(provide 'flycheck-languagetool)
;;; flycheck-languagetool.el ends here
