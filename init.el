;;; init.el --- Leo's personal Emacs config.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Commentary:
;;; Basic stuff like colors, layout, keybindings, etc.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
;;; Copy-pasted snippets have been attributed.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Directories:
;;; Global variables containing directories for custom major modes,
;;; config files, etc.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar org-path "/home/leo/Desktop/org/")
(defvar math-path "~/elisp/math-mode/")
(defvar awesome-path "~/.config/awesome/")
(defvar elisp-path "~/elisp/")
(defvar mathematica-path "~/.Mathematica/")
(add-to-list 'load-path elisp-path)
(add-to-list 'load-path math-path)

;; Save all backups to the same dir, instead of placing every backup in the
;; original file's directory. Taken from avdi's blog: http://bit.ly/qACeGz
(setq backup-directory-alist
      (list (cons "." (expand-file-name "backup" user-emacs-directory))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Gui:
;;; Transparency, bars, cursors.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(modify-all-frames-parameters '((alpha . 90)))
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(blink-cursor-mode 1)
(setq inhibit-startup-screen t)
(setq initial-scratch-message ";; Happy hacking Leo!\n\n")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Some random tweaks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Non-nil to allow minibuffer commands while in the minibuffer.
(setq enable-recursive-minibuffers t)

;; Truncate instead of wrap lines.
(set-default 'truncate-lines t)

;; Activate narrow-to-region.
(put 'narrow-to-region 'disabled nil)

;; This is just common sense.
(setq tab-width 4)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Aliased commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defalias 'qrr 'query-replace-regexp)

;; Every prompt for yes-or-no will now accept y-or-n (faster typing).
(defalias 'yes-or-no-p 'y-or-n-p)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Package management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(setq package-archives
      '(("marmalade" . "http://marmalade-repo.org/packages/")
	("ELPA" . "http://tromey.com/elpa/")
	("gnu" . "http://elpa.gnu.org/packages/")
	("melpa" . "http://melpa.milkbox.net/packages/")))

(defvar elpa-path "~/.emacs.d/elpa/")
(add-to-list 'load-path elpa-path)

(package-initialize)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Themes, fonts and visual stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq font-lock-maximum-decoration t)
(set-face-attribute 'default nil :height 180)

(show-paren-mode 1)
(setq-default fill-column 80)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'solarized-light t)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Language mode setups
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;
;;; Python
;;;;;;;;;;;;;;;

;; By default, python-mode splits the window vertically when opening the shell
;; or executing code from a buffer. These lines change that.
(setq-default py-split-windows-on-execute-function 'split-window-horizontally)
(setq-default py-split-window-on-execute t)

;; Python mode hooks
(add-hook 'python-mode-hook
	  (lambda ()
	    ;; Delete trailing whitespace at every save for the linter's sanity.
	    (add-to-list 'write-file-functions 'delete-trailing-whitespace)

	    ;; The default keybinding in python-mode to run the shell is C-c !
	    ;; which gets shadowed by Flycheck. Instead, we add a command to run
	    ;; the shell and jump into it, with an available key chord.
	    (define-key python-mode-map (kbd "C-c i") 'run-python)

	    ;; From comments inside python-mode: '[...]The specialized
	    ;; python-nav-forward-sexp allows easy navigation between code
	    ;; blocks. If you prefer cc-mode-like forward-sexp movement, setting
	    ;; forward-sexp-function to nil is enough[...]'
	    (setq forward-sexp-function nil)))

;; Thanks james! https://github.com/porterjamesj/virtualenvwrapper.el
(require 'virtualenvwrapper)

(add-hook 'venv-postactivate-hook
	  (lambda () (flycheck-buffer)))

(setq
 python-shell-interpreter "ipython"
 python-shell-interpreter-args "-i")

;;;;;;;;;;;;;;;;
;; Haskell
;;;;;;;;;;;;;;;;

(require 'haskell-mode)

;; Thanks: http://bit.ly/25SUWBV
(defun my-haskell-mode-hook ()
  "Setup correct indentation for Haskell mode."
   (haskell-indentation-mode -1)
   (haskell-indent-mode 1))

(add-hook 'haskell-mode-hook 'my-haskell-mode-hook)

;;;;;;;;;;;;;;;;
;; Others
;;;;;;;;;;;;;;;;

(require 'lua-mode)
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode))

(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.Mmd\\'" . markdown-mode))

(require 'Mathematica-mode)
(autoload 'math-edit-mode "math-edit-mode" "Mathematica editing mode." t)
(add-to-list 'auto-mode-alist '("\\.m\\'" . math-edit-mode))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Various major mode setups
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'ido)
(ido-mode t)

(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

;; my-favorite-files
(require 'mff)
(mff-add-files (("." . "~/elisp/init.el")
		("r" . (concat awesome-path "rc.lua"))
		("d" . (concat math-path "math-mode-defs.el"))
		("m" . (concat math-path "math-edit-mode.el"))
		("c" . (concat math-path "math-comint-mode.el"))
		("M" . (concat elisp-path "Mathematica-mode.el"))
		("y" . (concat elisp-path "mff.el"))
		("k" . (concat mathematica-path "Kernel/init.m"))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Various minor mode setups
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Flycheck for linter
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Yasnippets for code snippets
(yas-global-mode)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Window management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Replace other-window with ace-window
(global-set-key (kbd "C-x o") 'ace-window)

;; Force the flycheck error list window to always appear at the bottom
(add-to-list 'display-buffer-alist
             `(,(rx bos "*Flycheck errors*" eos)
               (display-buffer-reuse-window display-buffer-in-side-window)
               (side            . bottom)
               (window-height   . 0.3)))

;; Replace the default functionality of "C-c ! l" to open the list window and
;; select it.
(defun flycheck-list-errors-and-select ()
  "Display the list of errors and select the window."
  (interactive)
  (progn
    (flycheck-list-errors)
    (select-window (get-buffer-window "*Flycheck errors*"))))

(add-hook 'flycheck-mode-hook
	  (lambda ()
	    (define-key flycheck-mode-map (kbd "C-c ! l") 'flycheck-list-errors-and-select)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Custom text commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun insert-or-wrap (first last)
  "Do what I mean insertion of FIRST and LAST.
By default, insert the characters FIRST and LAST at point and
step back in between them.  If 'transient-mark-mode' is non-nil
and mark is active, wrap the region between start and end with
the characters FIRST and LAST and leave the mark at the first
one."
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Expansions / Completions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq hippie-expand-try-functions-list
      (quote (try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-all-abbrevs try-expand-dabbrev-from-kill
				 try-expand-line try-expand-list
				 try-complete-file-name try-complete-file-name-partially
				 try-complete-lisp-symbol-partially try-complete-lisp-symbol)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Global keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Done!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'init)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
