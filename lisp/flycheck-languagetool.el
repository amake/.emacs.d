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

(defun flycheck-languagetool-setup ()
  "Set up the flycheck-languagetool checker."
  (interactive)
  (add-to-list 'flycheck-checkers 'languagetool))

(provide 'flycheck-languagetool)
;;; flycheck-languagetool.el ends here
