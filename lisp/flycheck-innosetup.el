;;; flycheck-innosetup.el --- Lint Inno Setup scripts -*- lexical-binding: t -*-

;; Lint Inno Setup scripts; see http://www.jrsoftware.org/isinfo.php

;; Package-Requires: ((emacs "25.1") (flycheck "0.25"))

;;; Commentary:

;; Lint Inno Setup scripts.  Requires the `iscc' compiler to be available on the
;; path.  See `https://github.com/amake/innosetup-docker' for a containerized
;; version that can run outside of Windows.

;;; Code:

(require 'flycheck)

(flycheck-define-checker innosetup-iscc
  "An Inno Setup script checker.

See URL `http://www.jrsoftware.org/isinfo.php'."
  :command ("iscc" "/Q" "/O-" source-inplace)
  :error-patterns
  ((error line-start
          "Error on line " line " in Z:\\" (zero-or-more (not (in ":"))) ": Column " column ":\r\n"
          (message) "\r"
          line-end)
   (error line-start
          "Error on line " line " in Z:\\" (zero-or-more (not (in ":"))) ":" (message) "\r"
          line-end))
  :modes pascal-mode
  :predicate (lambda () (string= "iss" (file-name-extension (buffer-file-name)))))

(defun flycheck-innosetup-setup ()
  "Set up the flycheck-innosetup checker."
  (interactive)
  (add-to-list 'flycheck-checkers 'innosetup-iscc))

(provide 'flycheck-innosetup)

;;; flycheck-innosetup.el ends here
