;;; init.el --- My emacs setup -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;; Speed-up by temporarily disabling GC
;; See https://github.com/nilcons/emacs-use-package-fast
(let ((orig gc-cons-threshold))
  (setq gc-cons-threshold (* orig 80))
  (add-hook 'after-init-hook (lambda ()
                               ;; restore after startup
                               (setq gc-cons-threshold orig))))

;; Increase default font size.
(set-face-attribute 'default nil :height 180 :family "Menlo")

;; Fancy operator ligatures with Fira Code
(defvar amk-use-fancy-ligatures (fboundp #'mac-auto-operator-composition-mode))
(when amk-use-fancy-ligatures
  ;; Fira Code: https://github.com/tonsky/FiraCode
  (set-face-attribute 'default nil :family "Fira Code")
  (mac-auto-operator-composition-mode))

;; Set decent default fonts for Japanese and Chinese,
;; but *only* if in a graphical context.
;; Set Japanese second so that Japanese glyphs override Chinese
;; when both charsets cover the same codepoints.
(when (fboundp #'set-fontset-font)
  (set-fontset-font t 'han
                    ;; Noto Sans CJK: https://www.google.com/get/noto/help/cjk/
                    (font-spec :family "Noto Sans CJK SC"))
  (set-fontset-font t 'japanese-jisx0213.2004-1
                    ;; Source Han Code JP: https://github.com/adobe-fonts/source-han-code-jp
                    (font-spec :family "Source Han Code JP")))
(dolist (item '(("Source Han Code JP" . 1.25)
                ("Noto Sans CJK SC" . 1.25)))
  (add-to-list 'face-font-rescale-alist item))

;; Font scale test:
;; 0123456789|ABCDEFGHIJ|
;; ０１２３４|あいうえお|
;; 月光下，一|颗很小的蛋|躺在一个叶子上

(setq-default fill-column 80
              indent-tabs-mode nil
              tab-width 4
              mac-command-modifier 'super
              mac-option-modifier 'meta)

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
;; Override default http entry
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")))
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("org" . "https://orgmode.org/elpa/") t)
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

(defvar local-custom-file "~/.emacs.d/init-local.el"
  "A local version of CUSTOM-FILE for settings that should \
not be synced across machines.")
(when (file-exists-p local-custom-file)
  (load local-custom-file))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
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
  :load-path "lisp"
  :bind ("C-c C-o" . #'amk-multibrowse)
  :config
  (add-to-list 'amk-browse-alist '(backlog-issue-p . browse-backlog-issue)))

(use-package backlog
  :ensure nil
  :load-path "lisp/backlog"
  :after (request ivy)
  :bind ("C-c C-b" . #'browse-backlog-issue-at-point))

(use-package shfmt
  :ensure nil
  :load-path "lisp/shfmt"
  :ensure-system-package shfmt
  :custom
  (shfmt-arguments "-i 4")
  :hook (sh-mode . shfmt-enable-on-save))

(use-package desktop
  :if (display-graphic-p)
  :demand t
  :hook (auto-save-mode . (lambda ()
                            (if (eq (desktop-owner) (emacs-pid))
                                (desktop-save desktop-dirname))))
  :config
  (desktop-save-mode t))

(use-package shell
  :ensure nil
  :hook (shell-mode . (lambda ()
                        (shell-dirtrack-mode 0))))

(use-package image-mode
  :ensure nil
  :hook (image-mode . (lambda () (display-line-numbers-mode -1)))
  :config
  (use-package image-dimensions-minor-mode
    :ensure nil
    :load-path "lisp/wiki"))

(use-package flyspell
  :ensure-system-package (aspell . "sudo port install aspell-dict-en")
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
  (when (string= system-type "darwin")
    (setq dired-use-ls-dired nil)))

(use-package nxml-mode
  :ensure nil
  :custom
  (nxml-child-indent 4)
  :config
  (put 'nxml-child-indent 'safe-local-variable #'integerp)
  (put 'nxml-attribute-indent 'safe-local-variable #'integerp))

(use-package js
  :ensure nil
  :custom
  (js-indent-level 2))

(use-package browse-url
  :config
  (when (string= system-type "darwin")
    (setq browse-url-generic-program "open")))

(use-package org
  :ensure org-plus-contrib
  :custom
  (org-enforce-todo-checkbox-dependencies t)
  (org-enforce-todo-dependencies t)
  (org-startup-truncated nil "Wrap lines")
  (org-refile-targets '((org-agenda-files . (:maxlevel . 3))))
  (org-directory "~/org")
  (org-default-notes-file (concat (file-name-as-directory org-directory) "notes.org"))
  :hook (org-mode . (lambda () (setq-local amk-browse-fallback-action #'org-open-at-point)))
  :bind (;; Redefine here to override org-mode-map local definition
         ("C-c C-o" . amk-multibrowse)
         ;; Alternate mapping to avoid override by Flycheck
         ("C-c C-." . org-time-stamp-inactive))
  :config
  (when amk-use-fancy-ligatures
    ;; Table spacing sometimes gets messed up with Fira Code and Fira Mono for
    ;; some reason, but Menlo seems to be OK.
    (set-face-attribute 'org-table nil :family "Menlo"))
  (use-package org-agenda
    :ensure nil
    :bind ("C-c a" . org-agenda)
    :hook (org-agenda-mode . (lambda () (display-line-numbers-mode -1)))
    :config
    (let ((amk-agenda-files (concat (file-name-as-directory org-directory) "agenda")))
      (make-directory amk-agenda-files t)
      (add-to-list 'org-agenda-files amk-agenda-files)))
  (use-package org-capture
    :ensure nil
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
  ;; Backlog link support
  (use-package org-backlog
    :ensure nil
    :load-path "lisp/backlog")
  ;; pdfview link support
  (use-package org-pdfview))

(use-package magit
  :diminish (smerge-mode auto-revert-mode)
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch-popup)
         ("C-c l" . magit-log-buffer-file-popup)
         ("C-c b" . magit-blame-popup))
  :custom
  (magit-diff-refine-hunk 'all "Always show character-level diffs")
  (vc-handled-backends (delq 'Git vc-handled-backends) "Don't use VC for git")
  :config
  (global-magit-file-mode)
  (when (and amk-code-directory
             (file-exists-p amk-code-directory))
    (add-to-list 'magit-repository-directories `(,amk-code-directory . 1)))
  (use-package magit-svn
    ;;Do `git config --add magit.extension svn` to enable in repository
    ))

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
  :hook ((scss-mode . (lambda ()
                        (unless (executable-find "scss")
                          (async-shell-command "gem install sass")))))
  :config
  (global-flycheck-mode)
  (use-package flycheck-shfmt
    :ensure nil
    :load-path "lisp/shfmt"
    :config
    (flycheck-shfmt-setup)))

(use-package octave
  :ensure nil
  :mode (("\\.m\\'" . octave-mode))
  :custom (octave-block-offset 4)
  :config
  (put 'octave-block-offset 'safe-local-variable #'integerp))

(use-package counsel
  :diminish (ivy-mode counsel-mode)
  :ensure-system-package
  ((ag . "sudo port install the_silver_searcher")
   (rg . "sudo port install ripgrep"))
  :bind (("C-s" . swiper)
         ("C-c C-r" . ivy-resume)
         ("<f6>" . ivy-resume)
         ("<f1> l" . counsel-find-library)
         ("<f2> u" . counsel-unicode-char)
         ("C-c k" . counsel-rg)
         ("C-x l" . counsel-locate)
         ("C-S-o" . counsel-rhythmbox)
         :map minibuffer-local-map
         ("C-r" . counsel-minibuffer-history))
  :custom
  (ivy-use-virtual-buffers t)
  (enable-recursive-minibuffers t)
  :config
  (ivy-mode)
  (counsel-mode)
  (use-package all-the-icons-ivy
    :if (display-graphic-p)
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
            (groovy-mode all-the-icons-fileicon "groovy" :face all-the-icons-blue-alt)))
    (mapc (lambda (item) (add-to-list 'all-the-icons-icon-alist item))
          '(("\\.gradle$" all-the-icons-fileicon "gradle" :face all-the-icons-green)
            ("\\.groovy$" all-the-icons-fileicon "groovy" :face all-the-icons-blue-alt)))))

(use-package web-mode
  :mode ("\\.html?\\'"
         "\\.erb\\'"))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package dired-collapse
  :hook (dired-mode . dired-collapse-mode))

(use-package dash-at-point
  :bind ("C-c d" . dash-at-point))

(defun rename-buffer-with-project ()
  "Set the buffer name from the current Projectile project."
  (projectile-reset-cached-project-name)
  (let* ((proj (projectile-project-name))
         (buff (replace-regexp-in-string "\\(<[^>]+>\\)+$" "" (buffer-name)))
         (repl (if (string= proj "-")
                   buff
                 (format "%s<%s>" buff proj))))
    (rename-buffer repl t)))

(use-package projectile
  :demand t
  :ensure-system-package
  ((ag . "sudo port install the_silver_searcher")
   (rg . "sudo port install ripgrep"))
  :hook (shell-mode . rename-buffer-with-project)
  :bind-keymap ("C-c p" . projectile-command-map)
  :custom
  (projectile-mode-line '(:eval (when buffer-file-name
                                  (format " ‹%s›"
                                          (projectile-project-name)))))
  :config
  (projectile-mode)
  (use-package counsel-projectile
    :demand t
    :bind (("C-c j" . counsel-projectile-rg)
           ("C-c g" . counsel-projectile-find-file))
    :config
    (defun counsel-projectile-rg--no-tramp (old-function &rest args)
      (if (tramp-tramp-file-p (or (buffer-file-name)
                                  list-buffers-directory))
          (message "counsel-projectile-rg doesn't work over tramp")
        (apply old-function args)))
    (advice-add #'counsel-projectile-rg :around #'counsel-projectile-rg--no-tramp)
    (counsel-projectile-modify-action 'counsel-projectile-switch-project-action
                                      '((default "v")))
    (counsel-projectile-mode))
  (use-package ripgrep))

(use-package scala-mode
  :defer t)

(use-package play-routes-mode
  :mode "\\.routes\\'")

(use-package typescript-mode
  :defer t
  :custom
  (typescript-indent-level 2))

(use-package go-mode
  :defer t)

(use-package groovy-mode
  :defer t
  :hook (groovy-mode . (lambda ()
                         (local-unset-key (kbd "C-s")))))

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :ensure-system-package (python3 . "sudo port install python37")
  :config
  (use-package anaconda-mode
    :hook ((python-mode . anaconda-mode)
           (python-mode . anaconda-eldoc-mode))
    :ensure-system-package (easy_install-3.7 . "sudo port install py37-setuptools"))
  (use-package py-autopep8
    :ensure-system-package (autopep8 . "sudo port install py37-autopep8")
    :hook (python-mode . py-autopep8-enable-on-save)))

(use-package company-anaconda
  :after (company anaconda-mode)
  :config
  (add-to-list 'company-backends 'company-anaconda))

(use-package hideshow
  :diminish hs-minor-mode
  :bind ("C-c \\" . hs-toggle-hiding)
  :hook (prog-mode . hs-minor-mode)
  :config
  (add-to-list 'hs-special-modes-alist '(sh-mode "{" "}" "#" nil nil)))

(use-package dockerfile-mode
  :defer t)

(use-package docker-compose-mode
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
                          (pkg-config . "sudo port install pkgconfig")
                          (pdfinfo . "sudo port install poppler"))
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

(use-package vlf ;; View Large Files
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
  :config
  (use-package company-quickhelp
    :config
    (company-quickhelp-mode)))

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

(use-package auto-sudoedit
  :diminish auto-sudoedit-mode
  :config
  (auto-sudoedit-mode 1)
  (defun auto-sudoedit--skip-if-internal (old-function &rest args)
    (let ((path (auto-sudoedit-current-path)))
      (unless (or (emacs-internal-file-p path)
                  (python-system-file-p path))
       (apply old-function args))))
  (advice-add #'auto-sudoedit :around #'auto-sudoedit--skip-if-internal))

(use-package hide-lines
  :defer t)

(use-package flycheck-gradle
  :custom
  (flycheck-gradle-java-compile-function #'flycheck-gradle-java-compile->compile)
  :config
  (put 'flycheck-gradle-java-compile-function 'safe-local-variable #'functionp)
  (flycheck-gradle-setup))

(use-package apache-mode
  :mode "envvars\\'")

(use-package helpful
  :bind (("C-h f" . #'helpful-callable)
         ("C-h v" . #'helpful-variable)
         ("C-h k" . #'helpful-key)
         ("C-c C-d" . #'helpful-at-point)
         ("C-h F" . #'helpful-function)
         ("C-h C" . #'helpful-command)))

(provide 'init)
;;; init.el ends here
