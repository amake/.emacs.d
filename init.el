;;; package --- Summary

;;; Commentary:

;;; Code:

;; Increase default font size.
(set-face-attribute 'default nil :height 180 :family "Menlo")

;; Fancy operator ligatures with Fira Code
(when (fboundp 'mac-auto-operator-composition-mode)
  (set-face-attribute 'default nil :family "Fira Code")
  (mac-auto-operator-composition-mode))

;; Set decent default fonts for Japanese and Chinese,
;; but *only* if in a graphical context.
;; Set Japanese second so that Japanese glyphs override Chinese
;; when both charsets cover the same codepoints.
(when (fboundp 'set-fontset-font)
  (set-fontset-font
   t
   'chinese-gbk
   (font-spec :family "Noto Sans CJK SC"))
  (set-fontset-font
   t
   'japanese-jisx0213.2004-1
   (font-spec :family "Source Han Code JP")))
(dolist (item '(("Source Han Code JP" . 1.25)
                ("Noto Sans CJK SC" . 1.25)))
  (add-to-list 'face-font-rescale-alist item))

;; Font scale test:
;; 0123456789|ABCDEFGHIJ|
;; ０１２３４|あいうえお|
;; 月光下，一|颗很小的蛋|躺在一个叶子上

(setq custom-file "~/.emacs.d/custom-file.el")
(load custom-file)

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

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Eclipse-like line moving
;; https://www.emacswiki.org/emacs/MoveLine
(defmacro save-column (&rest body)
  "Note current column, do BODY, then restore column."
  (let ((tmp (make-symbol "col")))
    `(let ((,tmp (current-column)))
       ,@body
       (move-to-column ,tmp))))

(defun move-line-up ()
  "Swap the current line with the previous one."
  (interactive)
  (save-column
   (transpose-lines 1)
   (forward-line -2)))

(defun move-line-down ()
  "Swap the current line with the next one."
  (interactive)
  (save-column
   (forward-line 1)
   (transpose-lines 1)
   (forward-line -1)))

(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)

(defun open-pwd ()
  "Open the current working directory in the Finder."
  (interactive)
  (shell-command "open ."))

(defun open-file ()
  "Open the current file in the default app."
  (interactive)
  (shell-command (concat "open " (buffer-file-name))))

;; Only on GUI
(when (display-graphic-p)
  ;; Disable C-z (suspend-frame) in GUI because it's pointless
  ;; and I keep hitting it.
  (global-unset-key (kbd "C-z"))
  ;; Run server so `emacsclient` will edit in GUI editor.
  ;; May have to prepend /Applications/Emacs.app/Contents/MacOS/bin
  ;; to PATH on OS X.
  (server-start))

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

;; Bootstrap use-package
;; http://cachestocaches.com/2015/8/getting-started-use-package/
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)
(package-install-selected-packages)
(setq use-package-always-ensure t)

(use-package desktop
  :if (display-graphic-p)
  :config
  (desktop-save-mode t))

(use-package flyspell
  :config
  ;;(add-hook 'prog-mode-hook 'flyspell-prog-mode)
  (add-hook 'text-mode-hook 'flyspell-mode))

(use-package ediff
  :if window-system
  :config
  ;; https://www.ogre.com/node/446
  (add-hook 'ediff-before-setup-hook
            'make-frame)
  (add-hook 'ediff-quit-hook
            'delete-frame)
  (add-hook 'ediff-startup-hook
            '(lambda ()
               (set-frame-size (selected-frame) 175 55)
               (raise-frame (selected-frame)))))

(use-package dired
  :ensure nil
  :config
  ;; macOS ls doesn't support --dired
  (when (string= system-type "darwin")
    (setq dired-use-ls-dired nil)))

(use-package browse-url
  :bind ("C-c C-o" . browse-url-generic)
  :config
  (when (string= system-type "darwin")
    (setq browse-url-generic-program "open")))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config
  ;; Ensure emacs shell has regular shell environment
  (exec-path-from-shell-initialize))

(use-package org
  :ensure org-plus-contrib
  :bind ("C-c a" . org-agenda)
  :config
  ;; Backlog link support in org-mode
  (use-package org-backlog
    :ensure nil
    :load-path "org"))

;; Set magit shortcut
(use-package magit
  :diminish (smerge-mode auto-revert-mode)
  :bind ("C-x g" . magit-status)
  :config
  (setq magit-diff-refine-hunk 'all)
  (setq vc-handled-backends (delq 'Git vc-handled-backends)))

;; Use PCRE-style regex
(use-package pcre2el
  :diminish pcre-mode
  :config
  (pcre-mode))

;; On-the-fly linting
(use-package flycheck
  :config
  (global-flycheck-mode)
  ;; Set alternate key masked by flycheck
  (add-hook 'org-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-.") 'org-time-stamp-inactive))))

(use-package ivy
  :ensure counsel
  :diminish ivy-mode
  :bind (("C-s" . swiper)
         ("C-c C-r" . ivy-resume)
         ("<f6>" . ivy-resume)
         ("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("<f1> f" . counsel-describe-function)
         ("<f1> v" . counsel-describe-variable)
         ("<f1> l" . counsel-find-library)
         ("<f2> i" . counsel-info-lookup-symbol)
         ("<f2> u" . counsel-unicode-char)
         ("C-c g" . counsel-git)
         ("C-c j" . counsel-git-grep)
         ("C-c k" . counsel-ag)
         ("C-x l" . counsel-locate)
         ("C-S-o" . counsel-rhythmbox)
         :map read-expression-map
         ("C-r" . counsel-expression-history))
  :config
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (ivy-mode))

(use-package web-mode
  :mode "\\.html?\\'")

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package dired-collapse
  :config
  (add-hook 'dired-mode-hook 'dired-collapse-mode))

(use-package dash-at-point
  :bind ("C-c d" . dash-at-point))

(use-package projectile
  :diminish projectile-mode
  :config
  (projectile-mode)
  (use-package counsel-projectile
    :config
    (counsel-projectile-on)))

(use-package scala-mode
  :defer t)

(use-package play-routes-mode
  :mode "\\.routes\\'")

(use-package typescript-mode
  :defer t
  :config
  (setq typescript-indent-level 2))

(use-package go-mode
  :defer t)

(use-package groovy-mode
  :defer t
  :config
  (add-hook 'groovy-mode-hook (lambda ()
                                (local-unset-key (kbd "C-s")))))

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :config

  (use-package anaconda-mode
    :config
    (add-hook 'python-mode-hook 'anaconda-mode)
    (add-hook 'python-mode-hook 'anaconda-eldoc-mode))

  (use-package py-autopep8
    :config
    (add-hook 'python-mode-hook 'py-autopep8-enable-on-save)))

(provide 'init)
;;; init.el ends here
