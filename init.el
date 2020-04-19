;;; init.el --- My emacs setup -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;; Increase for better lsp-mode performance; see
;; https://github.com/emacs-lsp/lsp-mode#performance
(setq gc-cons-threshold 100000000)
(when (boundp 'read-process-output-max)
  ;; New in Emacs 27
  (setq read-process-output-max (* 1024 1024)))

;; Increase default font size.
(set-face-attribute 'default nil :height 180 :family "Menlo")

;; Fancy operator ligatures with Fira Code
(defconst amk-use-fancy-ligatures (fboundp #'mac-auto-operator-composition-mode))
(when amk-use-fancy-ligatures
  ;; Fira Code: https://github.com/tonsky/FiraCode
  (set-face-attribute 'default nil :family "Fira Code")
  (mac-auto-operator-composition-mode))

;; Set decent default fonts for Japanese and Chinese,
;; but *only* if in a graphical context.
;; Set Japanese second so that Japanese glyphs override Chinese
;; when both charsets cover the same codepoints.
(when (fboundp #'set-fontset-font)
  ;; Source Han Mono: https://github.com/adobe-fonts/source-han-mono
  ;; Source Han Code JP: https://github.com/adobe-fonts/source-han-code-jp
  (dolist (item '((hangul . "Source Han Mono K")
                  (han . "Source Han Mono SC")
                  (japanese-jisx0213.2004-1 . "Source Han Code JP")
                  (cyrillic . "Noto Sans Mono")
                  (greek . "Noto Sans Mono")
                  (hebrew . "Noto Sans Hebrew")
                  (thai . "Noto Sans Thai")
                  (arabic . "Noto Sans Arabic")
                  (bengali . "Noto Sans Bengali")
                  (devanagari . "Noto Sans Devanagari")
                  (kannada . "Noto Sans Kannada")
                  (malayalam . "Noto Sans Malayalam")
                  (gurmukhi . "Noto Sans Gurmukhi")
                  (tamil . "Noto Sans Tamil")
                  (telugu . "Noto Sans Telugu")))
    (set-fontset-font t (car item)
                      (font-spec :family (cdr item)))))
(dolist (item '(("Source Han Mono K" . 1.25)
                ("Source Han Mono JP" . 1.25)
                ("Source Han Code JP" . 1.25)
                ("Source Han Mono SC" . 1.25)
                ("Noto Sans Thai" . 1.25)
                ("Noto Sans Hebrew" . 1.2)
                ("Noto Sans Arabic" . 1.7)
                ("Noto Sans Bengali" . 1.35)
                ("Noto Sans Devanagari" . 1.55)
                ("Noto Sans Kannada" . 1.15)
                ("Noto Sans Malayalam" . 0.85)
                ("Noto Sans Gurmukhi" . 1.35)
                ("Noto Sans Tamil" . 0.9)
                ("Noto Sans Telugu" . 1.15)))
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
              save-interprogram-paste-before-kill t)

;; Workaround for failure when installing MacPorts packages via
;; system-packages-install
(unless (getenv "COLUMNS")
  (setenv "COLUMNS" "80"))

(setq custom-file "~/.emacs.d/custom-file.el")
(load custom-file)

(defvar amk-code-directory nil "Where I keep my programming projects.")

;; Make some emacs-app-mac keys match vanilla Emacs.app
(dolist (item '(("s-u" . revert-buffer)
                ("s-n" . make-frame)
                ("s-w" . delete-frame)
                ("s-a" . mark-whole-buffer)
                ("s-v" . yank)
                ("s-c" . kill-ring-save)
                ("s-x" . kill-region)
                ("s-q" . save-buffers-kill-emacs)
                ("s-s" . save-buffer)))
  (global-set-key (kbd (car item)) (cdr item)))

;; Don't intercept Japanese IME controls
(dolist (item '("C-S-j"
                "C-:"))
  (global-unset-key (kbd item)))

(show-paren-mode)
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
  (server-start))

(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/") ; ensure https
                         ("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")))
(package-initialize)

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
  ;; Ensure emacs shell has regular shell environment
  (exec-path-from-shell-initialize))

(use-package amk-edit
  :ensure nil
  :load-path "lisp"
  :bind (("M-<up>" . move-lines-up)
         ("M-<down>" . move-lines-down)
         ;; For use on CLI
         ("ESC <up>" . move-lines-up)
         ("ESC <down>" . move-lines-down)))

(use-package amk-macos
  :ensure nil
  :load-path "lisp")

(use-package amk-browse
  :ensure nil
  :disabled
  :load-path "lisp"
  :bind ("C-c C-o" . #'amk-multibrowse)
  :config
  (add-to-list 'amk-browse-alist '(backlog-issue-p . backlog-browse-issue)))

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
  :hook ((magit-status-mode . emoji-github-enable)
         (magit-log-mode . emoji-github-enable))
  :config
  (global-prettify-symbols-mode))

(use-package desktop
  :if (display-graphic-p)
  :demand t
  :hook (auto-save-mode . (lambda ()
                            (if (eq (desktop-owner) (emacs-pid))
                                (desktop-save desktop-dirname))))
  :config
  (desktop-save-mode t))

(use-package files
  :ensure nil
  :if (display-graphic-p)
  :custom
  (confirm-kill-emacs #'yes-or-no-p))

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
  :ensure-system-package (aspell . "sudo port install aspell aspell-dict-en")
  :hook ((text-mode . flyspell-mode)
         ;; (prog-mode . flyspell-prog-mode)
         ))

(use-package ediff
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

(use-package dired-x
  :ensure nil)

(use-package nxml-mode
  :ensure nil
  :hook (nxml-mode . maven-file-setup)
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
  :ensure-system-package ((npm . npm6)
                          (typescript-language-server . "sudo npm install -g typescript-language-server"))
  :hook (js-mode . lsp-deferred)
  :custom
  (js-indent-level 2))

(use-package browse-url
  :config
  (when (macosp)
    (setq browse-url-generic-program "open")))

(use-package org
  :ensure org-plus-contrib
  :custom
  (org-enforce-todo-checkbox-dependencies t)
  (org-enforce-todo-dependencies t)
  (org-startup-truncated nil "Wrap lines")
  (org-refile-targets '((nil . (:maxlevel . 1))
                        (org-agenda-files . (:maxlevel . 1))))
  (org-directory "~/org")
  (org-default-notes-file (concat (file-name-as-directory org-directory) "notes.org"))
  (org-src-tab-acts-natively t)
  :bind (
         ;; Alternate mapping to avoid override by Flycheck
         ("C-c C-." . org-time-stamp-inactive))
  :config
  (when amk-use-fancy-ligatures
    ;; Table spacing sometimes gets messed up with Fira Code and Fira Mono for
    ;; some reason, but Menlo seems to be OK.
    (set-face-attribute 'org-table nil :family "Menlo"))
  (org-babel-do-load-languages
   'org-babel-load-languages
   `(,@org-babel-load-languages
     (shell . t)
     (dot . t)
     (python . t)
     (ruby . t))))

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
(use-package org-pdfview
  :after org)

(use-package magit
  :diminish (smerge-mode auto-revert-mode)
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch)
         ("C-c L" . magit-log-buffer-file)
         ("C-c B" . magit-blame))
  :custom
  (magit-diff-refine-hunk 'all "Always show character-level diffs")
  (vc-handled-backends (delq 'Git vc-handled-backends) "Don't use VC for git")
  :config
  (global-magit-file-mode)
  (when (and amk-code-directory
             (file-exists-p amk-code-directory))
    (add-to-list 'magit-repository-directories `(,amk-code-directory . 1))))

(use-package magit-svn
  :after magit
  ;; Do `git config --add magit.extension svn` to enable in repository
  )

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

  ;; Do `git config --add forge.remote <remote>` to use something other than
  ;; `origin` as the target remote
  )

(use-package auth-source
  :ensure nil
  :config
  (when (macosp)
    (add-to-list 'auth-sources 'macos-keychain-internet)
    (add-to-list 'auth-sources 'macos-keychain-generic)))

(use-package git-gutter-fringe
  :if (display-graphic-p)
  :diminish git-gutter-mode
  :config
  (global-git-gutter-mode))

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
      (message "flycheck-mode disabled due to buffer size"))))

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
  :ensure-system-package ((pub . dart-sdk)
                          (dart-sass . "pub global activate sass"))
  :config
  (flycheck-dart-sass-setup))

(use-package octave
  :ensure nil
  :mode (("\\.m\\'" . octave-mode))
  :custom (octave-block-offset 4)
  :config
  (put 'octave-block-offset 'safe-local-variable #'integerp))

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

(use-package ruby-test-mode
  :after ruby-mode
  :hook (ruby-mode . ruby-test-mode)
  :diminish ruby-test-mode
  :config
  (defun ruby-test-rails-p (filename)
    "Return non-nil if FILENAME is part of a Ruby-on-Rails project."
    (ruby-test-rails-root filename))
  (defun ruby-test-rails-command (filename &optional line-number)
    "Return command to run test in FILENAME at LINE-NUMBER with Rails test runner."
    (let ((line-part (if line-number
                         (format ":%d" line-number)
                       "")))
      (format "PAGER=cat bundle exec rails test -v %s%s" filename line-part)))
  (defun ruby-test--command-with-rails (old-func &rest args)
    "Advise `ruby-test-command' to support the Ruby-on-Rails test runner."
    (let ((filename (car args)))
      (if (ruby-test-rails-p filename)
          (apply #'ruby-test-rails-command args)
        (apply old-func args))))
  (advice-add #'ruby-test-command :around #'ruby-test--command-with-rails))

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

(use-package all-the-icons-ivy
  :if (display-graphic-p)
  :after counsel
  :config
  (all-the-icons-ivy-setup)
  (mapc (lambda (item) (add-to-list 'all-the-icons-mode-icon-alist item))
        '((sh-mode all-the-icons-alltheicon "terminal" :face all-the-icons-purple)
          (conf-mode all-the-icons-fileicon "config" :face all-the-icons-yellow)
          (play-routes-mode all-the-icons-material "router" :face all-the-icons-dcyan)
          (Info-mode all-the-icons-material "info_outline")
          (tcl-mode all-the-icons-fileicon "tcl" :face all-the-icons-purple)
          (octave-mode all-the-icons-fileicon "octave" :face all-the-icons-cyan-alt)
          (sql-mode all-the-icons-fileicon "sqlite" :face all-the-icons-blue-alt)
          (package-menu-mode all-the-icons-octicon "package" :face all-the-icons-dyellow)
          (groovy-mode all-the-icons-fileicon "groovy" :face all-the-icons-blue-alt)
          (dart-mode all-the-icons-faicon "location-arrow" :face all-the-icons-blue-alt)
          (flycheck-error-list-mode all-the-icons-material "error_outline" :face all-the-icons-red)
          (tex-mode all-the-icons-fileicon "tex" :face all-the-icons-dyellow)
          (kotlin-mode all-the-icons-fileicon "kotlin" :face all-the-icons-blue)
          (restclient-mode all-the-icons-faicon "bed" :face all-the-icons-orange)))
  (mapc (lambda (item) (add-to-list 'all-the-icons-icon-alist item))
        '(("\\.gradle$" all-the-icons-fileicon "gradle" :face all-the-icons-green)
          ("\\.groovy$" all-the-icons-fileicon "groovy" :face all-the-icons-blue-alt))))

(use-package web-mode
  :mode ("\\.html?\\'"
         "\\.erb\\'"
         "\\.vue\\'")
  :ensure-system-package ((npm . npm6)
                          (vls . "sudo npm install -g vue-language-server"))
  :hook (web-mode . (lambda ()
                      (when (vue-file-p)
                        (lsp-deferred))))
  :config
  (defun vue-file-p ()
    (and buffer-file-name
         (string= (file-name-extension buffer-file-name) "vue"))))

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

(use-package ripgrep
  :after projectile)

(use-package scala-mode
  :defer t)

(use-package play-routes-mode
  :mode "\\.routes\\'")

(use-package typescript-mode
  :defer t
  :ensure-system-package ((npm . npm6)
                          (typescript-language-server . "sudo npm install -g typescript-language-server"))
  :hook (typescript-mode . lsp-deferred)
  :custom
  (typescript-indent-level 2))

(use-package go-mode
  :defer t
  :hook ((go-mode . (lambda ()
                      (add-hook 'before-save-hook #'gofmt-before-save nil t)))
         (go-mode . lsp-deferred))
  :ensure-system-package (go
                          (gopls . "GO111MODULE=on go get golang.org/x/tools/gopls@latest")))

(use-package groovy-mode
  :defer t
  :hook (groovy-mode . (lambda ()
                         (local-unset-key (kbd "C-s")))))

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :hook (python-mode . lsp-deferred)
  :ensure-system-package ((python3 . python37)
                          (pyls . py37-language-server)))

(use-package py-autopep8
  :after python
  :ensure-system-package (autopep8 . py37-autopep8)
  :hook (python-mode . py-autopep8-enable-on-save))

(use-package pyvenv
  :commands (pyvenv-activate pyvenv-workon))

(use-package hideshow
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
  (dumb-jump-mode)
  (mapc (lambda (item) (add-to-list 'dumb-jump-language-file-exts item))
        '((:language "typescript" :ext "ts" :agtype "ts" :rgtype "ts")
          (:language "typescript" :ext "tsx" :agtype "ts" :rgtype "ts")
          (:language "groovy" :ext "groovy" :agtype "groovy" :rgtype "groovy")
          (:language "groovy" :ext "gradle" :agtype "groovy" :rgtype "groovy")))
  (mapc (lambda (item) (add-to-list 'dumb-jump-language-comments item))
        '((:comment "//" :language "typescript")
          (:comment "//" :language "groovy")))
  (mapc (lambda (item) (add-to-list 'dumb-jump-find-rules item))
        ;; Rules translated from link below, except where noted
        ;; https://github.com/jacktasia/dumb-jump/issues/97#issuecomment-346441412
        ;;
        ;; --regex-typescript=/^[ \t]*(export[ \t]+(abstract[ \t]+)?)?class[ \t]+([a-zA-Z0-9_$]+)/\3/c,classes/
        '((:type "type" :supports ("ag" "grep" "rg" "git-grep") :language "typescript"
                 :regex "(export\\s+(abstract\\s+)?)?class\\s+JJJ\\b"
                 :tests ("class test" "export class test" "abstract class test"
                         "export abstract class test")
                 :not ("class testnot"))
          ;; --regex-typescript=/^[ \t]*(declare[ \t]+)?namespace[ \t]+([a-zA-Z0-9_$]+)/\2/c,modules/
          (:type "module" :supports ("ag" "grep" "rg" "git-grep") :language "typescript"
                 :regex "(declare\\s+)?namespace\\s+JJJ\\b"
                 :tests ("declare namespace test" "namespace test")
                 :not ("declare testnot"))
          ;; --regex-typescript=/^[ \t]*(export[ \t]+)?module[ \t]+([a-zA-Z0-9_$]+)/\2/n,modules/
          (:type "module" :supports ("ag" "grep" "rg" "git-grep") :language "typescript"
                 :regex "(export\\s+)?module\\s+JJJ\\b"
                 :tests ("export module test" "module test")
                 :not ("module testnot"))
          ;; --regex-typescript=/^[ \t]*(export[ \t]+)?(async[ \t]+)?function[ \t]+([a-zA-Z0-9_$]+)/\3/f,functions/
          (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "typescript"
                 :regex "(export\\s+)?(async\\s+)?function\\s+JJJ\\b"
                 :tests ("function test" "export function test" "export async function test"
                         "async function test")
                 :not ("function testnot"))
          ;;--regex-typescript=/^[ \t]*export[ \t]+(var|let|const)[ \t]+([a-zA-Z0-9_$]+)/\2/v,variables/
          (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "typescript"
                 :regex "export\\s+(var|let|const)\\s+JJJ\\b"
                 :tests ("export var test" "export let test" "export const test")
                 :not ("export var testnot"))
          ;; --regex-typescript=/^[ \t]*(var|let|const)[ \t]+([a-zA-Z0-9_$]+)[ \t]*=[ \t]*function[ \t]*[*]?[ \t]*\(\)/\2/v,varlambdas/
          (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "typescript"
                 :regex "(var|let|const)\\s+JJJ\\s*=\\s*function\\s*\\*?\\s*\\\(\\\)"
                 :tests ("var test = function ()" "let test = function()" "const test=function*()")
                 :not ("var testnot = function ()"))
          ;; --regex-typescript=/^[ \t]*(export[ \t]+)?(public|protected|private)[ \t]+(static[ \t]+)?(abstract[ \t]+)?(((get|set)[ \t]+)|(async[ \t]+[*]*[ \t]*))?([a-zA-Z1-9_$]+)/\9/m,members/
          (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "typescript"
                 :regex "(export\\s+)?(public|protected|private)\\s+(static\\s+)?(abstract\\s+)?(((get|set)\\s+)|(async\\s+))?JJJ\\b"
                 :tests ("public test" "protected static test" "private abstract get test"
                         "export public static set test" "export protected abstract async test")
                 :not ("public testnot"))
          ;; --regex-typescript=/^[ \t]*(export[ \t]+)?interface[ \t]+([a-zA-Z0-9_$]+)/\2/i,interfaces/
          (:type "type" :supports ("ag" "grep" "rg" "git-grep") :language "typescript"
                 :regex "(export\\s+)?interface\\s+JJJ\\b"
                 :tests ("interface test" "export interface test")
                 :not ("interface testnot"))
          ;; --regex-typescript=/^[ \t]*(export[ \t]+)?type[ \t]+([a-zA-Z0-9_$]+)/\2/t,types/
          (:type "type" :supports ("ag" "grep" "rg" "git-grep") :language "typescript"
                 :regex "(export\\s+)?type\\s+JJJ\\b"
                 :tests ("type test" "export type test")
                 :not ("type testnot"))
          ;; --regex-typescript=/^[ \t]*(export[ \t]+)?enum[ \t]+([a-zA-Z0-9_$]+)/\2/e,enums/
          (:type "type" :supports ("ag" "grep" "rg" "git-grep") :language "typescript"
                 :regex "(export\\s+)?enum\\s+JJJ\\b"
                 :tests ("enum test" "export enum test")
                 :not ("enum testnot"))
          ;; --regex-typescript=/^[ \t]*import[ \t]+([a-zA-Z0-9_$]+)/\1/I,imports/
          (:type "type" :supports ("ag" "grep" "rg" "git-grep") :language "typescript"
                 :regex "import\\s+JJJ\\b"
                 :tests ("import test")
                 :not ("import testnot"))
          ;; Custom definition for public methods without "public" keyword.
          ;; Fragile! Requires brace on same line.
          (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "typescript"
                 :regex "\\bJJJ\\s*\\(.*\\{"
                 :tests ("test() {" "test(foo: bar) {")
                 :not ("testnot() {"))
          ;; groovy (literally the same regexes as c#, but differents tests)
          (:type "function" :supports ("ag" "rg") :language "groovy"
                 :regex "^\\s*(?:[^=\\W]+\\s+){1,3}JJJ\\s*\\\("
                 :tests ("int test()" "int test(param)" "static int test()" "static int test(param)"
                         "public static MyType test()" "private virtual SomeType test(param)" "static int test()")
                 :not ("test()" "testnot()" "blah = new test()"))

          (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "groovy"
                 :regex "\\s*\\bJJJ\\s*=[^=\\n)]+" :tests ("int test = 1234") :not ("if test == 1234:" "int nottest = 44"))

          (:type "type" :supports ("ag" "grep" "rg" "git-grep") :language "groovy"
                 :regex "(class|interface)\\s*JJJ\\b"
                 :tests ("class test:" "public class test implements Something")
                 :not ("class testnot:" "public class testnot implements Something"))
          )))

(use-package pdf-tools
  :defer t
  :ensure-system-package (automake
                          autoconf
                          (pkg-config . pkgconfig)
                          (pdfinfo . poppler))
  :hook (pdf-view-mode . (lambda () (display-line-numbers-mode -1)))
  :config
  (pdf-tools-install))

(use-package paredit
  :diminish paredit-mode
  :hook ((emacs-lisp-mode . paredit-mode)
         (eval-expression-minibuffer-setup . paredit-mode)
         (ielm-mode . paredit-mode)
         (lisp-mode . paredit-mode)
         (lisp-interaction-mode . paredit-mode)
         (scheme-mode . paredit-mode))
  :bind (("M-S-<down>" . move-lines-down)
         ("M-S-<up>" . move-lines-up)))

(use-package swift-mode
  :defer t)

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
  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous))
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
  :hook (prog-mode . rainbow-mode))

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
    (let ((path (car args)))
      (unless (and (stringp path)
                   (or (emacs-internal-file-p path)
                       (python-system-file-p path)
                       (go-system-file-p path)))
        (apply old-function args))))
  (advice-add #'auto-sudoedit-should-activate :around #'auto-sudoedit--skip-if-internal))

(use-package hide-lines
  :defer t)

(use-package apache-mode
  :mode "envvars\\'")

(use-package helpful
  :bind (("C-h f" . #'helpful-callable)
         ("C-h v" . #'helpful-variable)
         ("C-h k" . #'helpful-key)
         ("C-c C-d" . #'helpful-at-point)
         ("C-h F" . #'helpful-function)
         ("C-h C" . #'helpful-command)))

(use-package blimp
  :custom
  (eimp-enable-undo t)
  :hook (image-mode . blimp-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :diminish (lsp-mode . "LSP")
  :bind (:map lsp-mode-map
              ("C-c C-a" . #'lsp-execute-code-action))
  :hook (lsp-mode . (lambda ()
                      (setq-local company-idle-delay 0.5)))
  :custom
  (lsp-file-watch-threshold nil)
  (lsp-solargraph-multi-root nil)
  :config
  (defun amk-lsp-format-on-save ()
    (add-hook 'before-save-hook #'lsp-format-buffer nil t))
  (defun amk-lsp-organize-imports-on-save ()
    (add-hook 'before-save-hook #'lsp-organize-imports nil t)))

(use-package lsp-ui
  :commands lsp-ui
  :after lsp-mode
  :hook (lsp-ui-doc-frame . (lambda (frame _window)
                              (with-selected-frame frame
                                (display-line-numbers-mode -1))))
  :custom
  (lsp-prefer-flymake nil)
  (lsp-ui-doc-position 'top)
  (lsp-ui-doc-alignment 'window))

(use-package company-lsp
  :commands company-lsp
  :after (company lsp-mode)
  :custom
  (company-lsp-cache-candidates 'auto))

(use-package lsp-java
  :after lsp-mode
  ;; Temporarily disabled for stability and performance issues
  ;; :hook (java-mode . lsp-deferred)
  :demand t)

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

(use-package restclient)

(use-package company-restclient
  :after restclient
  :hook (restclient-mode . (lambda ()
                             (make-local-variable 'company-backends)
                             (add-to-list 'company-backends 'company-restclient))))

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
  :ensure-system-package ((npm . npm6)
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
  :mode ("\\.arb\\'" . js-mode)
  :ensure-system-package ((npm . npm6)
                          (vscode-json-languageserver . "sudo npm install -g vscode-json-languageserver"))
  :hook (json-mode . lsp-deferred))

(provide 'init)
;;; init.el ends here
