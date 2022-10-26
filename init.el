;;; init.el --- My emacs setup -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;; Increase for better lsp-mode performance; see
;; https://emacs-lsp.github.io/lsp-mode/page/performance/
(setq gc-cons-threshold 100000000)
(when (boundp 'read-process-output-max)
  ;; New in Emacs 27
  (setq read-process-output-max (* 1024 1024)))

;; Set default font size and family.
;;
;; Disable font smoothing on Big Sur:
;;   defaults -currentHost write -g AppleFontSmoothing -int 0
(set-face-attribute 'default nil :height 180 :family "PragmataPro Liga")
(set-face-attribute 'fixed-pitch nil :height 180 :family "PragmataPro Liga")
(set-face-attribute 'variable-pitch nil :family "Neue Haas Grotesk Text Pro")

;; Fancy font when ligatures are available (Yamamoto Mitsuharu's Mac port)
(defconst amk-use-fancy-ligatures (fboundp #'mac-auto-operator-composition-mode))
(when amk-use-fancy-ligatures
  ;; Fira Code: https://github.com/tonsky/FiraCode
  ;; Iosevka: https://typeof.net/Iosevka/
  ;; PragmataPro: https://www.fsd.it/shop/fonts/pragmatapro/
  (set-face-attribute 'default nil :family "PragmataPro Liga") ; Or "Fira Code" or "Iosevka"
  (setq split-height-threshold 100)
  (mac-auto-operator-composition-mode))

;; Set decent default fonts for Japanese and Chinese,
;; but *only* if in a graphical context.
;; Set Japanese second so that Japanese glyphs override Chinese
;; when both charsets cover the same codepoints.
(when (fboundp #'set-fontset-font)
  ;; Source Han Mono: https://github.com/adobe-fonts/source-han-mono
  ;; Source Han Code JP: https://github.com/adobe-fonts/source-han-code-jp
  ;; Sarasa Gothic: https://github.com/be5invis/Sarasa-Gothic
  (pcase-dolist (`(,charset . ,family)
                 '((hangul . "Sarasa Gothic K") ; Or "Source Han Mono K"
                   (han . "Sarasa Gothic SC") ; Or "Source Han Mono SC"
                   (japanese-jisx0213.2004-1 . "Sarasa Gothic J") ; Or "Source Han Code JP"
                   (cyrillic . "PragmataPro Liga")
                   (greek . "PragmataPro Liga")
                   (hebrew . "PragmataPro Liga")
                   (thai . "Noto Sans Thai")
                   (arabic . "PragmataPro Liga")
                   (bengali . "Noto Sans Bengali")
                   (devanagari . "Noto Sans Devanagari")
                   (kannada . "Noto Sans Kannada")
                   (malayalam . "Noto Sans Malayalam")
                   (gurmukhi . "Noto Sans Gurmukhi")
                   (tamil . "Noto Sans Tamil")
                   (telugu . "Noto Sans Telugu")
                   (khmer . "Noto Sans Khmer")))
    (set-fontset-font t charset (font-spec :family family))))
(dolist (item '(("Source Han Mono K" . 1.25)
                ("Source Han Mono JP" . 1.25)
                ("Source Han Code JP" . 1.25)
                ("Source Han Mono SC" . 1.25)
                ("Noto Sans Arabic" . 1.35)
                ("Noto Sans Bengali" . 1.35)
                ("Noto Sans Devanagari" . 1.3)
                ("Noto Sans Gurmukhi" . 1.15)
                ("Noto Sans Khmer" . 1.25)))
  (add-to-list 'face-font-rescale-alist item))

;; Font scale test:
;; 0123456789|ABCDEFGHIJ|
;; ０１２３４|あいうえお|
;; 月光下，一|颗很小的蛋|躺在一个叶子上
;; 안녕, 세상|안녕, 세상|안녕, 세상

(setq-default fill-column 80
              indent-tabs-mode nil
              tab-width 4
              mac-command-modifier 'super
              mac-option-modifier 'meta
              require-final-newline t
              save-interprogram-paste-before-kill t
              sentence-end-double-space nil)

;; Workaround for failure when installing MacPorts packages via
;; system-packages-install
(unless (getenv "COLUMNS")
  (setenv "COLUMNS" "80"))

(setq custom-file "~/.emacs.d/custom-file.el")
(load custom-file)

(defvar amk-code-directory nil "Where I keep my programming projects.")

;; Make some emacs-app-mac keys match vanilla Emacs.app
(pcase-dolist (`(,key . ,func)
               '(("s-u" . revert-buffer)
                 ("s-n" . make-frame)
                 ("s-w" . delete-frame)
                 ("s-a" . mark-whole-buffer)
                 ("s-v" . yank)
                 ("s-c" . kill-ring-save)
                 ("s-x" . kill-region)
                 ("s-q" . save-buffers-kill-emacs)
                 ("s-s" . save-buffer)))
  (global-set-key (kbd key) func))

;; Don't intercept Japanese IME controls
(dolist (item '("C-S-j"
                "C-:"))
  (global-unset-key (kbd item)))

(column-number-mode)
(electric-pair-mode)
(delete-selection-mode)
(global-prettify-symbols-mode)

(add-hook 'before-save-hook #'delete-trailing-whitespace)

(defun save-buffer-no-hook (&optional arg)
  "Save buffer without invoking `before-save-hook', in case \
I want to save without deleting trailing whitespace.  ARG is as
with `save-buffer'."
  (interactive "p")
  (let (before-save-hook)
    (save-buffer arg)))

;; Only on GUI
(when (display-graphic-p)
  (if (fboundp #'global-display-line-numbers-mode)
      (global-display-line-numbers-mode) ; emacs 26 and later
    (global-linum-mode))
  ;; Disable C-z (suspend-frame) in GUI because it's pointless
  ;; and I keep hitting it.
  (global-unset-key (kbd "C-z"))
  ;; Run server so `emacsclient` will edit in GUI editor.
  ;; May have to prepend /Applications/Emacs.app/Contents/MacOS/bin
  ;; to PATH on OS X.
  (server-start)
  ;; New stuff
  ;;
  ;; Undo variable-pitch mode line
  (set-face-attribute 'mode-line nil :inherit 'default)
  ;; Pixel scrolling
  (when (fboundp #'pixel-scroll-precision-mode)
    (pixel-scroll-precision-mode)))

(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/") ; ensure https
                         ("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

;; Bootstrap use-package
;; http://cachestocaches.com/2015/8/getting-started-use-package/
(unless (package-installed-p 'use-package)
  (unless (string= (user-login-name) "root")
    (package-refresh-contents)
    (package-install 'use-package)))
(eval-when-compile
  (require 'use-package))
(unless (string= (user-login-name) "root")
  (package-install-selected-packages))
(setq use-package-always-ensure t)

(use-package diminish)
(use-package bind-key)
(use-package use-package-ensure-system-package)

(defconst local-custom-file "~/.emacs.d/init-local.el"
  "A local version of CUSTOM-FILE for settings that should \
not be synced across machines.")
(when (file-exists-p local-custom-file)
  (load local-custom-file))

(defun macosp ()
  "Return non-nil if the OS is macOS."
  (string= system-type "darwin"))

(use-package exec-path-from-shell
  :if (macosp)
  :config
  (dolist (var '(
                 ;; NS port doesn't pick up LC_ALL, which causes encoding
                 ;; errors in e.g. org-babel execution.
                 "LC_ALL"
                 ;; terraform-ls with tfenv needs this or it will try to
                 ;; install terraform to /opt/local.
                 "TFENV_CONFIG_DIR"))
    (add-to-list 'exec-path-from-shell-variables var))
  ;; Don't pick up MANPATH because I don't set it, and the value it determines
  ;; is missing MacPorts manpages.
  (setq exec-path-from-shell-variables
        (delete "MANPATH" exec-path-from-shell-variables))
  ;; Ensure emacs shell has regular shell environment
  (exec-path-from-shell-initialize))

(use-package amk-edit
  :ensure nil
  :load-path "lisp"
  :bind (("M-<up>" . amk-edit-move-lines-up)
         ("M-<down>" . amk-edit-move-lines-down)
         ;; For use on CLI
         ("ESC <up>" . amk-edit-move-lines-up)
         ("ESC <down>" . amk-edit-move-lines-down)))

(use-package amk-xml
  :ensure nil
  :load-path "lisp")

(use-package amk-mac
  :ensure nil
  :load-path "lisp")

(use-package amk-browse
  :ensure nil
  :disabled
  :load-path "lisp"
  :bind ("C-c C-o" . #'amk-browse-multi)
  :config
  (add-to-list 'amk-browse-alist '(backlog-issue-p . backlog-browse-issue)))

(use-package amk-org
  :ensure nil
  :load-path "lisp"
  :after (amk-edit org))

(use-package backlog
  :ensure nil
  :disabled
  :load-path "lisp/backlog"
  :after (request ivy)
  :bind ("C-c C-b" . #'backlog-browse-issue-at-point))

(use-package reformatter)

(use-package shfmt
  :ensure nil
  :load-path "lisp/shfmt"
  :after reformatter
  :ensure-system-package shfmt
  :diminish shfmt-on-save-mode
  :hook (sh-mode . shfmt-on-save-mode))

(use-package scale-to-fit
  :ensure nil
  :load-path "lisp")

(use-package emoji-github
  :ensure nil
  :load-path "lisp"
  :after magit
  :hook ((magit-status-mode magit-log-mode) . emoji-github-enable)
  :config
  (global-prettify-symbols-mode))

(use-package hl-tags-mode
  :ensure nil
  :load-path "lisp/hl-tags-mode"
  :after nxml-mode
  :hook (nxml-mode . hl-tags-mode))

(use-package macports
  :ensure nil
  :load-path "lisp/macports")

(use-package desktop
  :ensure nil
  :if (display-graphic-p)
  :demand t
  :hook (auto-save-mode . (lambda ()
                            (if (eq (desktop-owner) (emacs-pid))
                                (desktop-save desktop-dirname))))
  :custom
  (desktop-load-locked-desktop (if (>= emacs-major-version 29)
                                   'check-pid
                                 'ask))
  :config
  (desktop-save-mode t))

(use-package compile
  :ensure nil
  :custom
  (compilation-scroll-output 'first-error))

(use-package paren
  :ensure nil
  :custom
  (show-paren-context-when-offscreen 'overlay "From Emacs 29")
  :config
  (show-paren-mode))

(use-package files
  :ensure nil
  :if (display-graphic-p)
  :custom
  (confirm-kill-emacs #'yes-or-no-p))

(use-package lisp-mode
  :ensure nil
  :custom
  (emacs-lisp-docstring-fill-column t))

(use-package subword
  :ensure nil
  :diminish subword-mode
  :config
  (global-subword-mode))

(use-package eldoc
  :ensure nil
  :diminish eldoc-mode)

(use-package shell
  :ensure nil
  :hook (shell-mode . (lambda ()
                        (shell-dirtrack-mode 0))))

(use-package auto-revert
  :ensure nil
  :mode (("\\.log\\'" . auto-revert-tail-mode)))

(use-package image-mode
  :ensure nil
  :hook (image-mode . (lambda () (display-line-numbers-mode -1))))

(use-package cus-edit
  :ensure nil
  :hook (Custom-mode . (lambda () (display-line-numbers-mode -1))))

(use-package image-dimensions-minor-mode
  :ensure nil
  :after (image-mode blimp)
  :load-path "lisp/wiki"
  :config
  (advice-add #'eimp-replace-image :after #'image-dimensions-update-lighter))

(use-package flyspell
  :ensure nil
  :ensure-system-package (aspell . "sudo port -N install aspell aspell-dict-en")
  :hook ((text-mode . flyspell-mode)
         ;; (prog-mode . flyspell-prog-mode)
         ))

(use-package ediff
  :ensure nil
  :if (display-graphic-p)
  :custom
  ;; https://www.ogre.com/node/446
  (ediff-split-window-function #'split-window-horizontally)
  (ediff-window-setup-function #'ediff-setup-windows-plain))

(use-package dired
  :ensure nil
  :config
  ;; macOS ls doesn't support --dired
  (when (macosp)
    (setq dired-use-ls-dired nil)))

(use-package image-dired
  :ensure nil
  :hook (image-dired-thumbnail . (lambda () (display-line-numbers-mode -1))))

(use-package amk-dired-mac
  :ensure nil
  :load-path "lisp"
  :after (dired)
  :bind (:map dired-mode-map
              ("C-c C-o" . #'amk-dired-mac-open-in-finder)
              ("C-c C-r" . #'amk-dired-mac-reveal-in-finder)))

(use-package amk-dired
  :ensure nil
  :load-path "lisp"
  :after (dired)
  :bind (:map dired-mode-map
              ("C-c C-d" . #'amk-dired-ediff)))

(use-package dired-x
  :ensure nil)

(use-package nxml-mode
  :ensure nil
  :hook (nxml-mode . maven-file-setup)
  :mode (("\\.xaml\\'" . nxml-mode))
  :custom
  (nxml-child-indent 4)
  :config
  (put 'nxml-child-indent 'safe-local-variable #'integerp)
  (put 'nxml-attribute-indent 'safe-local-variable #'integerp)
  (defun maven-file-p ()
    (and buffer-file-name
         (string= (file-name-extension buffer-file-name) "xml")
         (save-excursion
           (re-search-forward "xmlns=['\"]http://maven.apache.org/[^/]+/[^'\"]+['\"]"
                              magic-mode-regexp-match-limit t))))
  (defun maven-file-setup ()
    (when (maven-file-p)
      (setq-local nxml-child-indent 2))))

(use-package js
  :ensure nil
  :after lsp-mode
  :ensure-system-package ((npm . npm8)
                          (typescript-language-server . "sudo npm install -g typescript-language-server")
                          (tsserver . "sudo npm install -g typescript"))
  :hook (js-mode . lsp-deferred)
  :custom
  (js-indent-level 2))

(use-package css-mode
  :ensure nil
  :custom
  (css-indent-offset 2))

(use-package browse-url
  :ensure nil
  :bind ("s-<mouse-1>" . #'browse-url-at-point)
  :config
  (when (macosp)
    (setq browse-url-generic-program "open")))

(use-package ob-passthrough
  :ensure nil
  :load-path "lisp")

(use-package org
  :ensure org-contrib
  :custom
  (org-enforce-todo-checkbox-dependencies t)
  (org-enforce-todo-dependencies t)
  (org-startup-truncated nil "Wrap lines")
  (org-refile-targets '((nil . (:maxlevel . 1))
                        (org-agenda-files . (:maxlevel . 1))))
  (org-directory "~/org")
  (org-default-notes-file (concat (file-name-as-directory org-directory) "notes.org"))
  (org-src-tab-acts-natively t)
  (org-log-done 'time)
  (org-preview-latex-default-process 'dvisvgm)
  (org-startup-with-inline-images t)
  (org-adapt-indentation t)
  :ensure-system-package dvisvgm
  :bind (
         ;; Alternate mapping to avoid override by Flycheck
         ("C-c C-." . org-time-stamp-inactive))
  :hook (org-babel-after-execute . org-redisplay-inline-images)
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   `(,@org-babel-load-languages
     (shell . t)
     (dot . t)
     (python . t)
     (ruby . t)
     (passthrough . t))))

(use-package tramp
  :ensure nil
  :custom
  (tramp-allow-unsafe-temporary-files t)
  :config
  ;; Make Tramp respect the remote server's PATH setting, so we can pick up
  ;; things like rbenv shims for remote Ruby execution via bundler
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

(use-package ob-async
  :hook (ob-async-pre-execute-src-block
         .
         ;; This can't be its own function because we'd have to redefine the
         ;; function in the other process, too
         (lambda ()
           (require 'tramp)
           (add-to-list 'tramp-remote-path 'tramp-own-remote-path))))

(use-package org-agenda
  :ensure nil
  :after org
  :bind ("C-c a" . org-agenda)
  :hook (org-agenda-mode . (lambda () (display-line-numbers-mode -1)))
  :config
  (let ((amk-agenda-path (concat (file-name-as-directory org-directory) "agenda")))
    (unless (file-directory-p amk-agenda-path)
      (make-directory amk-agenda-path t))
    (add-to-list 'org-agenda-files amk-agenda-path)))

(use-package org-capture
  :ensure nil
  :after org
  :bind ("C-c c" . org-capture)
  :config
  ;; Default template, not offered when custom templates are defined
  (add-to-list 'org-capture-templates
               '("t" "Task" entry (file+headline "" "Tasks") "* TODO %?\n  %u\n  %a"))
  ;; Templates for Firefox extension: https://github.com/sprig/org-capture-extension
  (add-to-list 'org-capture-templates
               '("p" "Protocol" entry (file+headline "" "Inbox")
                 "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?"))
  (add-to-list 'org-capture-templates
               '("L" "Protocol Link" entry (file+headline "" "Inbox")
                 "* %? [[%:link][%:description]] \nCaptured On: %U")))

(use-package ol
  :ensure nil
  :after org
  :bind ("C-c l" . org-store-link))

(use-package org-tempo
  :ensure nil
  :after org)

;; Backlog link support
(use-package org-backlog
  :ensure nil
  :after org
  :load-path "lisp/backlog")

;; pdfview link support
(use-package org-pdftools
  :after org)

(use-package magit
  :diminish (smerge-mode auto-revert-mode)
  :custom
  (magit-diff-refine-hunk 'all "Always show character-level diffs")
  :config
  (when (and amk-code-directory
             (file-exists-p amk-code-directory))
    (add-to-list 'magit-repository-directories `(,amk-code-directory . 1))))

(use-package forge
  ;; To store GitHub token in macOS keychain per `auth-source' config below:
  ;; 0. Create token at https://github.com/settings/tokens
  ;; 1. Open Keychain Access
  ;; 2. Create new password item
  ;; 3. Make sure it's recognized as an Internet password by entering
  ;;    https://api.github.com for the Keychain Item Name
  ;; 4. Use $username^forge as Account Name
  ;; 5. Enter token as password and save
  ;; 6. Test with `security find-internet-password -g -s api.github.com -a $username^forge'
  )

(use-package orgit-forge
  :after (org forge))

(use-package git-link
  :custom
  (git-link-use-commit t))

(use-package auth-source
  :ensure nil
  :config
  (when (macosp)
    (add-to-list 'auth-sources 'macos-keychain-internet)
    (add-to-list 'auth-sources 'macos-keychain-generic)))

(use-package gnus
  :ensure nil
  ;; To store Gmail auth info in macOS keychain per `auth-source' config above:
  ;; 0. Generate application-specific password
  ;; 1. Open Keychain Access
  ;; 2. Create new password item
  ;; 3. Make sure it's recognized as an Internet password by entering
  ;;    https://imap.googlemail.com:993 for the Keychain Item Name
  ;; 4. Use whatever as Account Name
  ;; 5. Enter password and save
  ;; 6. Test with `security find-internet-password -g -s imap.googlemail.com -P 993`
  ;; 7. Repeat with https://smtp.googlemail.com:587
  )

(use-package diff-hl
  :hook ((dired-mode . diff-hl-dired-mode-unless-remote)
         (magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  (face-spec-set
   'diff-hl-change
   '((((class color) (min-colors 88) (background dark)) :foreground "blue" :background "steelblue4")))
  (face-spec-set
   'diff-hl-delete
   '((((class color) (min-colors 88) (background dark)) :background "red4")))
  (face-spec-set
   'diff-hl-insert
   '((((class color) (min-colors 88) (background dark)) :foreground "green" :background "forestgreen")))
  (global-diff-hl-mode))

;; Use PCRE-style regex
(use-package pcre2el
  :diminish pcre-mode
  :config
  (pcre-mode))

;; On-the-fly linting
(use-package flycheck
  :hook (text-mode . amk-flycheck-disable-on-large-text)
  :config
  (global-flycheck-mode)
  (defun amk-flycheck-disable-on-large-text ()
    (when (> (buffer-size) 500000)
      (flycheck-mode -1)
      (message "flycheck-mode disabled due to buffer size")))
  ;; Temporary fix; remove pending merge of
  ;; https://github.com/flycheck/flycheck/pull/1793
  (defun flycheck-ruby--filter-errors (errors)
    "Filter Rubocop ERRORS attributed to dummy stdin filename."
    (flycheck-remove-error-file-names
     (flycheck--file-truename (expand-file-name "stdin"))
     errors))
  (flycheck-define-command-checker 'ruby-rubocop
    "A Ruby syntax and style checker using the RuboCop tool.

You need at least RuboCop 0.34 for this syntax checker.

See URL `http://batsov.com/rubocop/'."
    ;; ruby-standard is defined based on this checker
    :command '("rubocop"
               "--display-cop-names"
               "--force-exclusion"
               "--format" "emacs"
               ;; Explicitly disable caching to prevent Rubocop 0.35.1 and earlier
               ;; from caching standard input.  Later versions of Rubocop
               ;; automatically disable caching with --stdin, see
               ;; https://github.com/flycheck/flycheck/issues/844 and
               ;; https://github.com/bbatsov/rubocop/issues/2576
               "--cache" "false"
               (config-file "--config" flycheck-rubocoprc)
               (option-flag "--lint" flycheck-rubocop-lint-only)
               ;; Rubocop takes the original file name as argument when reading
               ;; from standard input, but it chokes when that name is the empty
               ;; string, so fall back to "stdin" in order to handle buffers with
               ;; no backing file (e.g. org-mode snippet buffers)
               "--stdin" (eval (or (buffer-file-name) "stdin")))
    :standard-input t
    :working-directory #'flycheck-ruby--find-project-root
    :error-patterns flycheck-ruby-rubocop-error-patterns
    :error-filter #'flycheck-ruby--filter-errors
    :modes '(enh-ruby-mode ruby-mode)
    :next-checkers '((warning . ruby-reek)
                     (warning . ruby-rubylint))))

(use-package flycheck-package
  :after flycheck
  :config
  (flycheck-package-setup))

(use-package flycheck-shfmt
  :ensure nil
  :after flycheck
  :load-path "lisp/shfmt"
  :config
  (flycheck-shfmt-setup))

(use-package flycheck-innosetup
  :ensure nil
  :after flycheck
  :load-path "lisp"
  :config
  (flycheck-innosetup-setup))

(use-package flycheck-languagetool
  :disabled ; Results are too frequently garbage
  :ensure nil
  :after flycheck
  :ensure-system-package languagetool
  :load-path "lisp"
  :custom
  (flycheck-languagetool-mother-tongue "en-US")
  (flycheck-languagetool-disable-rules-alist
   '((markdown-mode
      "WHITESPACE_RULE"
      "WORD_CONTAINS_UNDERSCORE"
      "COMMA_PARENTHESIS_WHITESPACE"
      "EN_QUOTES"
      "DASH_RULE")
     (t
      "WHITESPACE_RULE")))
  (flycheck-languagetool-line-by-line nil)
  :config
  (flycheck-languagetool-setup))

(use-package flycheck-sassc
  :ensure nil
  :after flycheck
  :load-path "lisp"
  :ensure-system-package sassc
  :config
  (flycheck-sassc-setup))

(use-package flycheck-dart-sass
  :ensure nil
  :after flycheck
  :load-path "lisp"
  :ensure-system-package (sass . dart-sass)
  :config
  (flycheck-dart-sass-setup))

(use-package flycheck-yard
  :ensure nil
  :after flycheck
  :load-path "lisp"
  :config
  (flycheck-yard-setup))

(use-package flycheck-committed
  :ensure nil
  :ensure-system-package committed
  :after flycheck
  :load-path "lisp"
  :config
  (flycheck-committed-setup))

(use-package flycheck-commitlint
  :ensure nil
  :ensure-system-package ((npm . npm8)
                          (commitlint . "sudo npm install -g @commitlint/cli @commitlint/config-conventional"))
  :after flycheck
  :load-path "lisp"
  :config
  (flycheck-commitlint-setup))

(use-package crdt)

(use-package cc-mode
  :ensure nil
  :config
  (defun objective-c-file-p ()
    (and buffer-file-name
         (string= (file-name-extension buffer-file-name) "m")
         (re-search-forward "@interface\\|@implementation"
                            magic-mode-regexp-match-limit t)))
  ;; Can't use use-package :magic because it only supports regexp
  (add-to-list 'magic-mode-alist '(objective-c-file-p . objc-mode)))

(use-package pascal
  :ensure nil
  :mode (("\\.iss\\'" . pascal-mode))
  :config
  (put 'pascal-indent-level 'safe-local-variable #'integerp))

(use-package ruby-mode
  :ensure nil
  :after lsp-mode
  :hook ((ruby-mode . lsp-deferred)
         (ruby-mode . amk-lsp-format-on-save))
  :custom
  (ruby-insert-encoding-magic-comment nil "Not needed in Ruby 2")
  :ensure-system-package (solargraph . "gem install --user-install solargraph"))

(use-package inf-ruby)

(use-package ruby-test-mode
  :after ruby-mode
  :diminish ruby-test-mode
  :config
  (defun amk-ruby-test-pretty-error-diffs (old-func &rest args)
    "Make error diffs prettier."
    (let ((exit-status (cadr args)))
      (apply old-func args)
      (when (> exit-status 0)
        (diff-mode)
        ;; Remove self
        (advice-remove #'compilation-handle-exit #'amk-ruby-test-pretty-error-diffs))))
  (defun amk-ruby-test-pretty-error-diffs-setup (old-func &rest args)
    "Set up advice to enable pretty diffs when tests fail."
    (advice-add #'compilation-handle-exit :around #'amk-ruby-test-pretty-error-diffs)
    (apply old-func args))
  (advice-add #'ruby-test-run-command :around #'amk-ruby-test-pretty-error-diffs-setup))

(use-package counsel
  :demand t
  :diminish (ivy-mode counsel-mode)
  :ensure-system-package (rg . ripgrep)
  :bind (("C-s" . swiper)
         ("C-r" . swiper-backward)
         ("C-c C-r" . ivy-resume)
         ("<f6>" . ivy-resume)
         ("<f1> l" . counsel-find-library)
         ("<f2> u" . counsel-unicode-char)
         ("C-c k" . amk-counsel-rg-here)
         ("C-x l" . counsel-locate)
         ("C-S-o" . counsel-rhythmbox)
         ("C-c h" . counsel-git-log)
         :map minibuffer-local-map
         ("C-r" . counsel-minibuffer-history))
  :custom
  (ivy-use-virtual-buffers t)
  (enable-recursive-minibuffers t)
  :config
  (ivy-mode)
  (counsel-mode)
  (defun amk-counsel-rg-here ()
    "Like `counsel-rg' but always searches from the cwd, not project root."
    (interactive)
    (counsel-rg nil default-directory)))

(use-package all-the-icons
  :if (display-graphic-p)
  :after counsel
  :config
  (mapc (lambda (item) (add-to-list 'all-the-icons-mode-icon-alist item))
        '((conf-mode all-the-icons-fileicon "config" :face all-the-icons-yellow)
          (play-routes-mode all-the-icons-material "router" :face all-the-icons-dcyan)
          (Info-mode all-the-icons-material "info_outline")
          (octave-mode all-the-icons-fileicon "octave" :face all-the-icons-cyan-alt)
          (sql-mode all-the-icons-fileicon "sqlite" :face all-the-icons-blue-alt)
          (package-menu-mode all-the-icons-octicon "package" :face all-the-icons-dyellow)
          (groovy-mode all-the-icons-fileicon "groovy" :face all-the-icons-blue-alt)
          (flycheck-error-list-mode all-the-icons-material "error_outline" :face all-the-icons-red)
          (restclient-mode all-the-icons-faicon "bed" :face all-the-icons-orange)))
  (mapc (lambda (item) (add-to-list 'all-the-icons-extension-icon-alist item))
        '(("\\.groovy$" all-the-icons-fileicon "groovy" :face all-the-icons-blue-alt))))

(use-package all-the-icons-ivy-rich
  :ensure t
  :config
  (all-the-icons-ivy-rich-mode))

(use-package ivy-rich
  :ensure t
  :config
  (ivy-rich-mode))

(use-package all-the-icons-ibuffer
  :ensure t
  :config
  (all-the-icons-ibuffer-mode))

(use-package all-the-icons-dired
  :diminish all-the-icons-dired-mode
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package web-mode
  :mode ("\\.html?\\'"
         "\\.erb\\'"
         "\\.vue\\'"
         "\\.tsx\\'")
  :ensure-system-package ((npm . npm8)
                          (vls . "sudo npm install -g vls"))
  :after prettier
  :hook (web-mode . (lambda ()
                      (when (or (react-file-p) (vue-file-p))
                        (lsp-deferred)
                        (prettier-mode))))
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-enable-auto-indentation nil)
  :config
  (defun react-file-p ()
    (and buffer-file-name
         (string= (file-name-extension buffer-file-name) "tsx")))
  (defun vue-file-p ()
    (and buffer-file-name
         (string= (file-name-extension buffer-file-name) "vue"))))

(use-package prettier
  :diminish prettier-mode
  :ensure-system-package ((npm . npm8)
                          (prettier . "sudo npm install -g prettier"))
  :hook ((typescript-mode . prettier-mode)
         (svelte-mode . prettier-mode)))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package dired-collapse
  :hook (dired-mode . dired-collapse-mode))

(use-package dash-at-point
  :bind ("C-c d" . dash-at-point))

(defun rename-buffer-with-project ()
  "Set the buffer name from the current Projectile project."
  (let* ((proj (projectile-project-name))
         (buff (replace-regexp-in-string "\\(<[^>]+>\\)+$" "" (buffer-name)))
         (repl (if (string= proj "-")
                   buff
                 (format "%s<%s>" buff proj))))
    (rename-buffer repl t)))

(use-package projectile
  :demand t
  :ensure-system-package (rg . ripgrep)
  :hook (shell-mode . rename-buffer-with-project)
  :bind-keymap ("C-c p" . projectile-command-map)
  :custom
  (projectile-mode-line-prefix "")
  (projectile-mode-line-function (lambda ()
                                   (when buffer-file-name
                                     (format " ‹%s›"
                                             (projectile-project-name)))))
  :config
  (projectile-mode))

(use-package counsel-projectile
  :demand t
  :after (counsel projectile)
  :bind (("C-c j" . counsel-projectile-rg)
         ("C-c g" . counsel-projectile-find-file))
  :config
  (defun counsel-projectile-rg--no-tramp (old-function &rest args)
    (let ((path (or (buffer-file-name)
                    list-buffers-directory)))
      (if (tramp-tramp-file-p path)
          (let* ((vec (tramp-dissect-file-name default-directory))
                 (host (tramp-file-name-host vec))
                 (local (tramp-file-name-localname vec)))
            (if (string= host (system-name))
                ;; If it's a tramp buffer BUT the file is a local one (using
                ;; tramp for sudo) then we can still make things work by
                ;; pretending for this invocation that we aren't in tramp.
                (let ((default-directory local))
                  (apply old-function args))
              (message "counsel-projectile-rg doesn't work over tramp")))
        (apply old-function args))))
  (advice-add #'counsel-projectile-rg :around #'counsel-projectile-rg--no-tramp)
  (counsel-projectile-modify-action 'counsel-projectile-switch-project-action
                                    '((default "v")))
  (counsel-projectile-mode))

(use-package edit-string
  :ensure nil
  :demand t
  :load-path "lisp"
  :bind (:map
         prog-mode-map
         ("C-c '" . edit-string-at-point))
  :config
  (defun amk-sql-p ()
    (re-search-forward "\\bSELECT\\b" nil t))
  (add-to-list 'edit-string-guess-mode-alist '(amk-sql-p . sql-mode)))

(use-package typescript-mode
  :defer t
  :after edit-string
  :ensure-system-package ((npm . npm8)
                          (typescript-language-server . "sudo npm install -g typescript-language-server"))
  :bind ( ; Alternate binding to avoid clobber via `edit-string-at-point'
         ("C-c C-'" . typescript-convert-to-template)
         ("C-c '" . edit-string-at-point))
  :hook (typescript-mode . lsp-deferred)
  :custom
  (typescript-autoconvert-to-template-flag t)
  (typescript-indent-level 2))

(use-package jest-test-mode
  :hook (typescript-mode js-mode typescript-tsx-mode)
  :diminish (jest-test-mode)
  :config
  (put 'jest-test-command-string 'safe-local-variable #'stringp))

(use-package svelte-mode
  :defer t
  :ensure-system-package ((npm . npm8)
                          (svelteserver . "sudo npm install -g svelte-language-server"))
  :hook (svelte-mode . lsp-deferred))

(use-package go-mode
  :defer t
  :hook ((go-mode . (lambda ()
                      (add-hook 'before-save-hook #'gofmt-before-save nil t)))
         (go-mode . lsp-deferred))
  :ensure-system-package gopls)

(use-package groovy-mode
  :defer t
  :hook (groovy-mode . (lambda ()
                         (local-unset-key (kbd "C-s")))))

(use-package python
  :ensure nil
  :mode ("\\.py\\'" . python-mode)
  :hook (python-mode . lsp-deferred)
  :ensure-system-package ((python3 . python310)
                          (pylsp . py310-python-lsp-server)))

(use-package py-autopep8
  :after python
  :ensure-system-package (autopep8 . py310-autopep8)
  :hook (python-mode . py-autopep8-mode))

(use-package pyvenv
  :commands (pyvenv-activate pyvenv-workon))

(use-package hideshow
  :ensure nil
  :diminish hs-minor-mode
  :bind ("C-c \\" . hs-toggle-hiding)
  :hook (prog-mode . hs-minor-mode)
  :config
  (add-to-list 'hs-special-modes-alist '(sh-mode "{" "}" "#" nil nil)))

(use-package dockerfile-mode
  :defer t)

(use-package markdown-mode
  :defer t
  :custom
  (markdown-command "multimarkdown")
  :ensure-system-package multimarkdown)

(use-package dumb-jump
  :custom
  (dumb-jump-selector 'ivy)
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package pdf-tools
  :defer t
  :ensure-system-package (automake
                          autoconf
                          (pkg-config . pkgconfig)
                          (pdfinfo . poppler))
  :hook (pdf-view-mode . (lambda () (display-line-numbers-mode -1)))
  :config
  (pdf-tools-install t))

(use-package paredit
  :diminish paredit-mode
  :hook ((emacs-lisp-mode
          eval-expression-minibuffer-setup
          ielm-mode
          lisp-mode
          lisp-interaction-mode
          scheme-mode)
         . paredit-mode)
  :bind (("M-S-<down>" . amk-edit-move-lines-down)
         ("M-S-<up>" . amk-edit-move-lines-up)
         ("M-<right>" . paredit-forward-slurp-sexp)
         ("M-<left>" . paredit-forward-barf-sexp)))

(use-package swift-mode
  :defer t)

(use-package lsp-sourcekit
  :after lsp-mode
  :hook (swift-mode . lsp-deferred)
  :custom
  (lsp-sourcekit-executable
   "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/sourcekit-lsp"))

(use-package free-keys
  :commands free-keys)

(use-package request)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package vlf ; View Large Files
  :config
  (require 'vlf-setup))

(use-package company
  :diminish company-mode
  :hook (after-init . global-company-mode)
  :custom
  (company-idle-delay 0.1)
  (company-dabbrev-downcase nil)
  (company-dabbrev-ignore-case t))

(use-package company-quickhelp
  :after company
  :config
  (company-quickhelp-mode))

(use-package rainbow-mode
  :diminish rainbow-mode
  :hook ((prog-mode conf-mode) . rainbow-mode))

(defun emacs-internal-file-p (path)
  "Return non-nil if PATH represents a file that might be part of the Emacs installation."
  (let ((invocation-directory-parent (expand-file-name (concat invocation-directory ".."))))
    (string-prefix-p invocation-directory-parent path)))

(defun python-system-file-p (path)
  "Return non-nil if PATH represents a file that might be part of a Python system installation."
  (string-match-p ".*/Python.framework/.*" path))

(defun go-system-file-p (path)
  "Return non-nil if PATH represents a file that might be part of a Go system installation."
  (string-match-p ".*/go/src/.*" path))

(use-package auto-sudoedit
  :diminish auto-sudoedit-mode
  :config
  (auto-sudoedit-mode 1)
  (defun auto-sudoedit--skip-if-internal (old-function &rest args)
    (let ((path (auto-sudoedit-current-path)))
      (unless (and (stringp path)
                   (or (emacs-internal-file-p path)
                       (python-system-file-p path)
                       (go-system-file-p path)))
        (apply old-function args))))
  (advice-add #'auto-sudoedit :around #'auto-sudoedit--skip-if-internal))

(use-package hide-lines
  :defer t)

(use-package apache-mode
  :mode "envvars\\'")

(use-package nginx-mode)

(use-package helpful
  :bind (("C-h f" . #'helpful-callable)
         ("C-h v" . #'helpful-variable)
         ("C-h k" . #'helpful-key)
         ("C-c C-d" . #'helpful-at-point)
         ("C-h F" . #'helpful-function)
         ("C-h C" . #'helpful-command))
  :config
  (when (string= emacs-version "29.0.50")
    ;; Remove when fixed upstream; see
    ;; https://github.com/Wilfred/elisp-refs/issues/35
    ;; https://github.com/Wilfred/helpful/issues/282
    (defvar read-symbol-positions-list nil)
    (defun helpful--autoloaded-p (sym buf)
      "Return non-nil if function SYM is autoloaded."
      (-when-let (file-name (buffer-file-name buf))
        (setq file-name (s-chop-suffix ".gz" file-name))
        (help-fns--autoloaded-p sym)))
    (defun helpful--skip-advice (docstring)
      "Remove mentions of advice from DOCSTRING."
      (let* ((lines (s-lines docstring))
             (relevant-lines
              (--take-while
               (not (or (s-starts-with-p ":around advice:" it)
                        (s-starts-with-p "This function has :around advice:" it)))
               lines)))
        (s-trim (s-join "\n" relevant-lines))))))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :diminish (lsp-mode . "LSP")
  :bind (:map lsp-mode-map
              ("C-c C-a" . #'lsp-execute-code-action))
  :hook (lsp-mode . lsp-enable-which-key-integration)
  :custom
  (lsp-file-watch-threshold nil)
  (lsp-solargraph-multi-root nil)
  :config
  (defun amk-lsp-format-buffer-quick ()
    (let ((lsp-response-timeout 2))
      (lsp-format-buffer)))
  (defun amk-lsp-format-on-save ()
    (add-hook 'before-save-hook #'amk-lsp-format-buffer-quick nil t))
  (defun amk-lsp-disable-format-on-save ()
    (remove-hook 'before-save-hook #'amk-lsp-format-buffer-quick t))
  (defun amk-lsp-organize-imports-on-save ()
    (add-hook 'before-save-hook #'lsp-organize-imports nil t)))

(use-package lsp-ui
  :commands lsp-ui
  :after lsp-mode
  :hook (lsp-ui-doc-frame . amk-lsp-ui-doc-customize)
  :custom
  (lsp-prefer-flymake nil)
  (lsp-ui-doc-position 'top)
  (lsp-ui-doc-alignment 'window)
  :config
  (defun amk-lsp-ui-doc-customize (frame _window)
    (let ((foreground (frame-parameter (selected-frame) 'foreground-color)))
      (with-selected-frame frame
        (display-line-numbers-mode -1)
        ;; lsp-ui overrides `after-make-frame-functions' so we have to fix up
        ;; the appearance here
        (set-foreground-color foreground))))
  (advice-add #'amk-mac-apply-appearance-mode :after (lambda (&rest _)
                                                       (lsp-ui-doc--delete-frame)))
  (lsp-ui-doc--delete-frame))

(use-package lsp-ivy
  :after lsp-mode
  :bind (:map lsp-mode-map
              ("C-c s" . #'lsp-ivy-workspace-symbol)))

(use-package lsp-java
  :after lsp-mode
  ;; Temporarily disabled for stability and performance issues
  ;; :hook (java-mode . lsp-deferred)
  :demand t
  :ensure-system-package ((mvn . maven3)
                          ("/Library/Java/JavaVirtualMachines/openjdk11" . openjdk11))
  :custom
  (lsp-java-java-path "/Library/Java/JavaVirtualMachines/openjdk11/Contents/Home/bin/java"))

(use-package dart-mode
  :after (lsp-dart scale-to-fit)
  :diminish flutter-test-mode
  :hook ((dart-mode . lsp-deferred)
         (dart-mode . amk-lsp-format-on-save)
         (dart-mode . amk-lsp-organize-imports-on-save)
         (dart-mode . dart-scale-text-to-fit)
         (dart-mode . flutter-test-mode))
  :ensure-system-package (dart . dart-sdk)
  :custom
  (lsp-dart-sdk-dir "/Applications/flutter/bin/cache/dart-sdk/")
  :config
  (defun dart-scale-text-to-fit ()
    "Adjust text scale to fit for Dart files."
    (scale-to-fit-setup 80 -2 0)))

(use-package lsp-dart
  :after lsp-mode)

(use-package flutter
  :ensure nil
  :load-path "lisp/flutter"
  :ensure-system-package (pod . "gem install --user-install cocoapods")
  :after dart-mode
  :bind (:map dart-mode-map
              ("C-M-x" . #'flutter-run-or-hot-reload))
  :custom
  (flutter-sdk-path "/Applications/flutter/"))

(use-package vue-l10n
  :ensure nil
  :load-path "lisp")

(use-package flutter-l10n-flycheck
  :ensure nil
  :load-path "lisp/flutter"
  :after (flutter flycheck)
  :config
  (flutter-l10n-flycheck-setup))

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode))

(use-package kotlin-mode)

(use-package flycheck-kotlin
  :ensure-system-package ktlint
  :config
  (flycheck-kotlin-setup))

(use-package yasnippet
  ;; Snippets in ~/.emacs.d/snippets by default
  :diminish yas-minor-mode
  :config
  (yas-global-mode))

(use-package beacon
  :diminish beacon-mode
  :custom
  (beacon-blink-duration 0.1)
  (beacon-blink-delay 0.1)
  :config
  (beacon-mode)
  ;; This variable isn't initialized yet at :custom eval time,
  ;; so set it here
  (setq beacon-dont-blink-major-modes
        (delq 'magit-status-mode beacon-dont-blink-major-modes)))

(use-package jq-mode)

(use-package cmake-mode)

(use-package yaml-mode
  :ensure-system-package ((npm . npm8)
                          (yaml-language-server . "sudo npm install -g yaml-language-server"))
  :hook ((yaml-mode . lsp-deferred)
         (yaml-mode . flyspell-prog-mode))
  :config
  (remove-hook 'yaml-mode-hook #'flyspell-mode))

(use-package treemacs
  :hook (treemacs-mode . (lambda () (display-line-numbers-mode -1)))
  :bind (:map global-map
              ("M-0" . treemacs-select-window)
              ("C-x t 1" . treemacs-delete-other-windows)
              ("C-x t t" . treemacs)
              ("C-x t B" . treemacs-bookmark)
              ("C-x t C-t" . treemacs-find-file)
              ("C-x t M-t" . treemacs-find-tag)
              ("C-x t s" . treemacs-switch-workspace)
              :map treemacs-mode-map
              ("S" . treemacs-switch-workspace)))

(use-package lsp-treemacs
  :after (treemacs lsp-mode))

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package json-mode
  :after lsp-mode
  :mode ("\\.arb\\'" . json-mode)
  :bind (:map js-mode-map ("M-." . nil))
  :ensure-system-package ((npm . npm8)
                          (vscode-json-languageserver . "sudo npm install -g vscode-json-languageserver"))
  :hook (json-mode . lsp-deferred))

(use-package vterm
  :ensure-system-package (cmake
                          ("/opt/local/lib/libvterm.dylib" . libvterm))
  :custom ((vterm-shell "/bin/zsh --login")
           (vterm-buffer-name-string "vterm %s")
           (vterm-max-scrollback 2000)
           (vterm-always-compile-module t)))

(use-package amk-vterm
  :ensure nil
  :load-path "lisp"
  :after (projectile vterm)
  :bind (("C-z" . amk-vterm-for-project)
         :map vterm-mode-map
         ("M-z" . amk-vterm-for-project)))

(use-package csharp-mode
  :after lsp-mode
  ;; Omnisharp has errors with Emacs; see
  ;; https://github.com/OmniSharp/omnisharp-roslyn/issues/1689
  ;; :hook (csharp-mode . lsp-deferred)
  )

(use-package csv-mode)

(use-package editorconfig
  :diminish editorconfig-mode
  :config
  (editorconfig-mode 1))

(use-package docker
  :hook ((docker-container-mode
          docker-image-mode
          docker-network-mode
          docker-volume-mode)
         . (lambda () (display-line-numbers-mode -1))))

(use-package terraform-mode
  :after lsp-mode
  :ensure-system-package terraform-ls
  :custom
  (lsp-terraform-ls-enable-show-reference t)
  :hook ((terraform-mode . lsp-deferred)
         (terraform-mode . terraform-format-on-save-mode)))

(use-package coffee-mode)

(use-package pug-mode)

(use-package nodejs-repl
  :custom
  (nodejs-repl-arguments '("--experimental-repl-await")))

(use-package edit-indirect)

(provide 'init)
;;; init.el ends here
