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
(global-set-key (kbd "s-u") 'revert-buffer)
(global-set-key (kbd "s-n") 'make-frame)
(global-set-key (kbd "s-w") 'delete-frame)
(global-set-key (kbd "s-a") 'mark-whole-buffer)
(global-set-key (kbd "s-v") 'yank)
(global-set-key (kbd "s-c") 'kill-ring-save)
(global-set-key (kbd "s-x") 'kill-region)
(global-set-key (kbd "s-q") 'save-buffers-kill-emacs)

;; Ensure emacs shell has regular shell environment
;; via exec-path-from-shell package.
(package-initialize)
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; Set up spelling
(setq ispell-program-name "aspell")
(add-hook 'text-mode-hook 'flyspell-mode)
;;(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(setq-default indent-tabs-mode nil)

(show-paren-mode)
(column-number-mode)

;; Only on GUI
(when window-system
  ;; Restore session
  (desktop-save-mode t)
  ;; Disable C-z (suspend-frame) in GUI because it's pointless
  ;; and I keep hitting it.
  (global-unset-key (kbd "C-z"))
  ;; Run server so `emacsclient` will edit in GUI editor.
  ;; May have to prepend /Applications/Emacs.app/Contents/MacOS/bin
  ;; to PATH on OS X.
  (server-start)
  ;; ediff customizations
  ;; https://www.ogre.com/node/446
  (add-hook 'ediff-before-setup-hook
            'make-frame)
  (add-hook 'ediff-quit-hook
            'delete-frame)
  (add-hook 'ediff-startup-hook
            '(lambda ()
               (set-frame-size (selected-frame) 175 55)
               (raise-frame (selected-frame)))))

;; Don't intercept Japanese IME controls
(global-unset-key (kbd "C-S-j"))
(global-unset-key (kbd "C-:"))

;; Set magit shortcut
(global-set-key (kbd "C-x g") 'magit-status)

;; Add melpa package repo
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)

;; Improve text printing speed in Python shell
(setq python-shell-enable-font-lock nil)

;; Wrap lines in org-mode
(setq org-startup-truncated nil)

;; Backlog link support in org-mode
(load "~/.emacs.d/org-backlog.el")
(setq org-backlog-team "rxdev")

;; Set org-agenda stuff
(global-set-key (kbd "C-c a") 'org-agenda)

;; Set alternate key masked by flycheck
(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-.") 'org-time-stamp-inactive)))

;; org-mode hooks for TaskJuggler
;; (from org-plus-contrib package; see also `port info taskjuggler`)
(require 'ox-taskjuggler)

;; Use PCRE-style regex
(pcre-mode)

;; On-the-fly linting
(require 'flycheck)
(global-flycheck-mode)

(provide 'init)
;;; init.el ends here
