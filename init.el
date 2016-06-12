;;; init.el --- personal Emacs config

;;; Commentary:
;; Basic stuff: colors, layout, keybindings, etc.

;;; Code:

;;;;;;;;;;;;;;;
;;; Directories
;;;;;;;;;;;;;;;
(defvar org-path "/home/leo/Desktop/org/")
(defvar math-path "~/elisp/math-mode/")
(defvar awesome-path "~/.config/awesome/")
(defvar elisp-path "~/elisp/")
(defvar elpa-path "~/.emacs.d/elpa/")
(defvar mathematica-path "~/.Mathematica/")
(add-to-list 'load-path elisp-path)
(add-to-list 'load-path elpa-path)
(add-to-list 'load-path math-path)

;; save all backups to the same dir
;; taken from avdi's blog: http://bit.ly/qACeGz
(setq backup-directory-alist
      (list (cons "." (expand-file-name "backup" user-emacs-directory))))

;;;;;;;;;;;;;;;;;;;;;;;
;;; Gui & startup stuff
;;;;;;;;;;;;;;;;;;;;;;;
(modify-all-frames-parameters '((alpha . 90)))
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(blink-cursor-mode 1)
(setq inhibit-startup-screen t)
(setq initial-scratch-message ";; Happy hacking Leo!\n\n")

;;;;;;;;;;;;;;;;;;;;;;
;;; Some random tweaks
;;;;;;;;;;;;;;;;;;;;;;
(setq enable-recursive-minibuffers t)
(set-default 'truncate-lines t)
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")
(put 'narrow-to-region 'disabled nil)
(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'qrr 'query-replace-regexp)
(setq tab-width 4)


;;;;;;;;;;;;
;;; Packages
;;;;;;;;;;;;
(require 'package)
(setq package-archives
      '(("marmalade" . "http://marmalade-repo.org/packages/")
	("ELPA" . "http://tromey.com/elpa/")
	("gnu" . "http://elpa.gnu.org/packages/")
	("melpa" . "http://melpa.milkbox.net/packages/")))
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Themes & Visual stuff
;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'solarized-light t)
(setq font-lock-maximum-decoration t)
(show-paren-mode 1)
(set-face-attribute 'default nil :height 180)
(setq-default fill-column 80)

;;;;;;;;;;;;;;;;;;;;;;;
;;; Various mode setups
;;;;;;;;;;;;;;;;;;;;;;;
;;; Social
;; (require 'twittering-mode)
;; (setq twittering-use-master-password t)
;; (setq twittering-icon-mode t)
;; (setq twittering-timer-interval 300)
;; (setq twittering-url-show-status nil)

;; (require 'jabber)
;; (set-face-attribute 'jabber-activity-face nil		:foreground "dark orange"	:weight 'bold)
;; (set-face-attribute 'jabber-activity-personal-face nil	:foreground "deep sky blue"	:weight 'bold)
;; (set-face-attribute 'jabber-roster-user-online	nil	:foreground "peru"		:weight 'normal)
;; (set-face-attribute 'jabber-chat-prompt-foreign	nil	:foreground "light steel blue"	:weight 'bold)
;; (set-face-attribute 'jabber-chat-prompt-local nil	:foreground "OliveDrab1"	:weight 'bold)
;; (setq jabber-vcard-avatars-retrieve t)
;; (setq jabber-roster-line-format " %a %-25n")
;; (setq jabber-chat-buffer-format "*%n-chat*")
;; (setq jabber-alert-message-hooks '(jabber-message-awesome jabber-message-scroll))

;;; Languages and linters
(require 'python-mode)
(setq py-python-command "python3")
;; (setq python-shell-interpreter "/usr/bin/python3.5")
(setq python-shell-interpreter "/usr/bin/python2.7")
(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python3" . python-mode))
(add-hook 'python-mode-hook
	  (lambda ()
		(add-to-list 'write-file-functions 'delete-trailing-whitespace)
		(define-key python-mode-map (kbd "C-c i") 'py-shell)))


(require 'lua-mode)
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode))

(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.Mmd\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(require 'Mathematica-mode)
(autoload 'math-edit-mode "math-edit-mode" "Mathematica editing mode." t)
(add-to-list 'auto-mode-alist '("\\.m\\'" . math-edit-mode))

(require 'haskell-mode)
(add-hook 'haskell-mode-hook 'my-haskell-mode-hook)

;; Thanks: https://stackoverflow.com/questions/4887452/forcing-haskell-indent-mode-over-haskell-indentation-mode-in-haskell-mode-2-7
(defun my-haskell-mode-hook ()
  "Setup correct indentation for Haskell mode."
   (haskell-indentation-mode -1)
   (haskell-indent-mode 1))

;; Linter and snippets
(add-hook 'after-init-hook #'global-flycheck-mode)
(yas-global-mode)


;;; Misc
;; (require 'paredit)
(require 'ido)
(ido-mode t)

(require 'mff)
(mff-add-files (("." . "~/elisp/init.el")
		("t" . (concat org-path "todo.org"))
		;; ("a" . (concat org-path "agenda.org"))
		("r" . (concat awesome-path "rc.lua"))
		("d" . (concat math-path "math-mode-defs.el"))
		("m" . (concat math-path "math-edit-mode.el"))
		("c" . (concat math-path "math-comint-mode.el"))
		("M" . (concat elisp-path "Mathematica-mode.el"))
		("y" . (concat elisp-path "mff.el"))
		("k" . (concat mathematica-path "Kernel/init.m"))))

;;; Org
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

;;;;;;;;;;;;;;;;;;;
;;; Custom commands
;;;;;;;;;;;;;;;;;;;
(defun insert-or-wrap (first last)
  "Do what I mean insertion of FIRST and LAST.
Usually, insert the characters FIRST and LAST at point and steps
back in between them.  If 'transient-mark-mode' is non-nil and
mark is active, wrap the region between start and end with the
characters FIRST and LAST and leave the mark at the first one."
  (if (and mark-active transient-mark-mode)
      (save-restriction
	;; narrow is necessary because we are inserting chars
	(narrow-to-region (region-beginning) (region-end))
	(goto-char (point-min)) ; leaves the mark here
	(insert first)
	(goto-char (point-max))
	(insert last))
    (insert (concat first last))
    (backward-char (length last))))

(defun kill-whitespace ()
  "Kill the whitespace between two non-whitespace characters."
  (interactive "*")
  (save-excursion
    (save-restriction
      (save-match-data
	(progn
	  (re-search-backward "[^ \t\r\n]" nil t)
	  (re-search-forward "[ \t\r\n]+" nil t)
	  (replace-match "" nil nil))))))

(setq hippie-expand-try-functions-list
      (quote (try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-all-abbrevs try-expand-dabbrev-from-kill
				 try-expand-line try-expand-list
				 try-complete-file-name try-complete-file-name-partially
				 try-complete-lisp-symbol-partially try-complete-lisp-symbol)))

;;;;;;;;;;;;;;;
;;; Keybindings
;;;;;;;;;;;;;;;
(global-set-key (kbd "C-ñ") mff-mode-map)
(global-set-key (kbd "C-x C-b") 'ibuffer-other-window)
(global-set-key (kbd "C-.") (lambda () (interactive) (hippie-expand nil)))
(global-set-key (kbd "C-+") 'kill-whitespace)
(global-set-key (kbd "C-z") 'shell)
(global-set-key (kbd "M-z") 'end-of-buffer)
(global-set-key (kbd "M-Z") 'beginning-of-buffer)
(global-set-key (kbd "H-h") (lambda () (interactive) (insert-or-wrap "<" ">")))
(global-set-key (kbd "H-j") (lambda () (interactive) (insert-or-wrap "(" ")")))
(global-set-key (kbd "H-k") (lambda () (interactive) (insert-or-wrap "[" "]")))
(global-set-key (kbd "H-l") (lambda () (interactive) (insert-or-wrap "{" "}")))
(global-set-key (kbd "H-ñ") (lambda () (interactive) (insert-or-wrap "\"" "\"")))
(global-set-key (kbd "H-p") (lambda () (interactive) (insert-or-wrap "'" "'")))
(global-set-key (kbd "H-o") (lambda () (interactive) (insert-or-wrap "`" "`")))
(global-set-key (kbd "H-i") (lambda () (interactive) (insert-or-wrap "```\n" "\n```")))




(provide 'init)
;;; init.el ends here
