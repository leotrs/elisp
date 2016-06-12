;;
;; A mode to edit Mathematica code in emacs
;;
;; math-mode-definitios
;; Provides simple tools for editing Mathematica code:
;; + syntax hihglithing
;; + comment wrapping
;; + support for camel case (by using subword-mode)
;; + simple indentation scheme
;;
;; math-*-mode should derive their specifications from these
;; definitions

;;;;;;;;;;;;;;;;;;
;; Auxiliary stuff
;;;;;;;;;;;;;;;;;;
;; Variables
(defvar math-mode-path "~/elisp/math-mode/"
  "*Directory in which math-mode files are stored.")

;; Functions
(defun chars-behind-point (n)
  "Returns the n chars immediately before point.
If there are fewer than n characters between point and beginning
of buffer, return them all."
  (buffer-substring-no-properties
   (point) (max (- (point) n) 1)))

(defun maybe-backward-up-list ()
  "If point is inside an sexp, goes back and outside of it and
returns t, otherwise returns nil and leaves point where it was."
  (let ((beg (point)))
    (ignore-errors (backward-up-list))
    (if (= beg (point))	nil t)))

(defmacro make-regexp-from-file (name sym)
  "Reads the file <name> from disk and makes a regexp from the
words found there. If the file contains words (in syntax-table
sense), they must be ordered by one word per line and <sym>
should be the quoted symbol 'words. If the contents of the file
are not words but other strings, <sym> should be nil. Stores the
result in <name>-regexp."
  `(defvar ,(intern (concat name "-regexp"))
     (regexp-opt
      (with-temp-buffer
	(insert-file-contents (concat ,math-mode-path ,name))
	(split-string (buffer-string) "\n" t))
      ,sym)
     ,(concat "Regexp that matches some Mathematica " name ".")))

(defmacro erase-regexp (name)
  "Easy deletion of regexps created with make-regexp-from-file.
Sets <name>-regexp to nil."
  `(setq ,(intern (concat name "-regexp")) nil))

;;;;;;;;;;;;;;;;;;;;;;;
;; Keyword highlighting
;;;;;;;;;;;;;;;;;;;;;;;
(make-regexp-from-file "functions" 'words)
(make-regexp-from-file "constants" 'words)
(make-regexp-from-file "keywords" 'words)
(make-regexp-from-file "specials" nil)
(defvar type-regexp
  (regexp-opt '("Integer" "Real" "Complex" "Rational" "Symbol" "String") 'words)
  "Regexp that matches the atomic types in Mathematica.")
(defvar builtins-regexp "\\$[a-zA-Z]*"
  "Regexp that matches the builtin system constants in Mathematica.")
(defvar string-regexp "\"[^\"]*\\(\\\"\\)*\"?"
  "Regexp that matches string constants in Mathematica.")
(defvar comment-regexp "(\\*.*?\\(\\*)\\)+"
  "Regexp that matches comments in Mathematica.")
(defvar variable-regexp "\\<\\w+?\\(\\[\\w*\\]\\)?_+"
  "Regexp that matches variables in function definitions in Mathematica,
i.e., it matches x_, etc.")
(defvar delayed-rule-regexp "\\([a-zA-Z][a-zA-Z0-9]*\\)\\[.*\\][ \t]*:?="
  "Regexp that matches global delayed rules in Mathematica.")

(defvar math-mode-font-locks
  ;; Order matters: first match rules. The (optional) second
  ;; numeric item indicates which of the match's caputres
  ;; should be coloured
  `((,string-regexp . font-lock-string-face)
    (,comment-regexp . font-lock-comment-face)
    (,type-regexp . font-lock-type-face)
    (,builtins-regexp . font-lock-builtin-face)
    (,constants-regexp . font-lock-constant-face)
    (,variable-regexp . font-lock-variable-name-face)
    (,functions-regexp . font-lock-function-name-face)
    (,keywords-regexp . font-lock-keyword-face)
    (,specials-regexp . font-lock-keyword-face)    
    (,delayed-rule-regexp 1 font-lock-function-name-face))
  "Syntax highlighting for Mathematica mode.")

;;;;;;;;;;;;;;;
;; Syntax table
;;;;;;;;;;;;;;;
(defvar math-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; flag meanings:
    ;; .: punctuation
    ;; (c: open delimiter that matches c
    ;; )c: close delimiter that matches c
    ;; 1: start of a two character start comment sequence
    ;; 2: second character in said comment sequence
    ;; 3: start of a two character end comment sequence
    ;; 4: second of a two character end comment sequence
    ;; n: comments with this delimeter can be nested
    ;; w: word constituent
    (modify-syntax-entry ?\( "(). 1" table)
    (modify-syntax-entry ?* ". 23n" table)
    (modify-syntax-entry ?\) ")(. 4" table)
    (modify-syntax-entry ?\& "." table)
    (modify-syntax-entry ?\# "w." table)
    table))

;;;;;;;;;;;;;;
;; Indentation
;;;;;;;;;;;;;;
(defvar math-tab-size 3
  "*Default tab size for Mathematica code.")

(defvar math-max-special-symbol-length 3
  "*The maximum length of special symbols. See the file specials
  for the list of symbols.")

(defun math-skip-comments-backward ()
  "Skips Mathematica comments backward. Jumps preceding
expressions until a non comment expression is found."
  (let (should-skip-comment)
    (save-excursion
      (skip-chars-backward "\t\s\n")
      (setq should-skip-comment
	    (and (not (= (point) 0))
		 (string= (chars-behind-point (length comment-end))
			  comment-end))))
    (when should-skip-comment
      (ignore-errors (backward-sexp))
      (math-skip-comments-backward))))

(defun math-normal-indent ()
  "Indent a normal line of code, i.e., not after a special
symbol. See the file math-mode/specials for the list."
  (save-excursion
    (beginning-of-line)
    (if (not (maybe-backward-up-list))
	0
      ;; else indent based on nature of head
      (let ((head (thing-at-point 'word)))
	(if (or (not head)
		(not (string-match keywords-regexp head)))
	    (1+ (current-column))
	  (ignore-errors (backward-sexp))
	  (+ (current-column) math-tab-size))))))

(defun math-special-indent (symbol)
  "Indent a line after a special symbol. Receives the symbol
after which we are indenting, as a string."
  (save-excursion
    (back-to-indentation)
    (math-skip-comments-backward)
    (skip-chars-backward "\t\s\n")
    (if (maybe-backward-up-list)
	(+ (current-column) math-tab-size)
      (backward-char (length symbol))
      (backward-sexp)
      (+ (current-indentation) math-tab-size))))

(defun math-calculate-indent ()
  "Calculate the indentation of the current line. Basically
decides whether the line is the first line after a special symbol
or not and passes control to math-special-indent or
math-normal-indent, respectively."
  (let (special-symbol str)
    (save-excursion
      (beginning-of-line)
      (math-skip-comments-backward)
      (skip-chars-backward "\t\s\n")
      (setq str (chars-behind-point math-max-special-symbol-length))
      (setq special-symbol
	    (if (string-match (concat specials-regexp "$") str)
		(match-string 0 str)
	      nil)))
    ;; recuperate point and mark
    (if special-symbol (math-special-indent special-symbol) (math-normal-indent))))

(defun math-indent-line ()
  "Indent current line as Mathematica code."
  (let ((new-indent (math-calculate-indent)))
    (if (not (= new-indent (current-indentation)))
	(save-excursion
	  (back-to-indentation)
	  (let ((beg (point)))
	    (beginning-of-line)
	    (delete-region beg (point))
	    (indent-to new-indent)))))
  (if (< (current-column) (current-indentation))
      (back-to-indentation)))

;;;;;;;;;;;;;;;;;;;;
;; Commands & Keymap
;;;;;;;;;;;;;;;;;;;;
(defun math-kill-sexp (arg)
  "Kill from point to the end of current exp. Intended to replace
kill-sexp."
  (interactive "*p")
  (save-excursion
    (let ((beg (point))
	  (times (if arg (abs arg) 1)))
      (if (or (not arg) (> arg 0))
	  (dotimes (i times)
	    (skip-chars-forward "a-zA-Z0-9")
	    (ignore-errors (forward-sexp)))
	;; negative argument: note the order change
	(dotimes (i times)
	  (ignore-errors (backward-sexp))
	  (skip-chars-backward "a-zA-Z0-9")))
      (kill-region beg (point)))))

(defun math-send-input (input)
  "Send the string input to the kernel."
  (when (not math-comint-buffer)
    (math-comint-start t)
    (accept-process-output (get-process "Mathematica Interpreter"))
    (accept-process-output (get-process "Mathematica Interpreter")))
  (with-current-buffer  math-comint-buffer
    ;; math-comint-buffer is created by math-comint-start
    (goto-char (point-max))
    (insert input)
    (comint-send-input)
    (goto-char (point-max))))

(defun math-send-region (start end)
  "Send the active region as input to a running Mathematica
Kernel."
  (interactive "r")
  (math-send-input (buffer-substring-no-properties start end)))

(defun math-doc-at-point ()
  "Requests the Mathematica kernel for the documentation of the
symbol at point."
  (interactive)
  (math-send-input (concat "?" (thing-at-point 'word))))

(defun math-send-dwim ()
  "Do what I mean: send input to the kernel buffer."
  (interactive)
  (save-excursion
    (let (beg end)
      (if (and mark-active transient-mark-mode)
	  (setq beg (region-beginning) end (region-end))
	(setq end (point))
	(backward-sexp 2)
	(setq beg (point)))
      (setq math-inter-mark end)
      (math-send-region beg end))))

(defun math-append-postfix ()
  "Go to the end of line and insert a postfix operator '//'."
  (interactive)
  (end-of-line)
  (insert " // "))

(defmacro math-make-keys (map)
  "Common code for every keymap used in a math-mode based mode.
This is done to be able to inherit from multiple keymaps. See for
example, math-comint-mode-map. See also math-make-mode."
  `(progn
     (define-key map [remap kill-sexp] 'math-kill-sexp)
     (define-key map (kbd "C-c C-s") 'math-send-dwim)
     (define-key map (kbd "C-c C-e") 'math-append-postfix)
     (define-key map (kbd "C-c C-d") 'math-doc-at-point)))

;;;;;;;;;;;;;;;;;;
;; Mode definition
;;;;;;;;;;;;;;;;;;
(defmacro math-make-mode (mode-name)
  "Common code for every math-mode based mode. This is done so as
to be able to inherit from different parent modes. For example,
math-commint-mode inherits from comint-mode and math-edit-mode
inherits from fundamental. However, they both share this
code. See also math-make-keys."
  `(progn
     (set (make-local-variable 'font-lock-defaults) '(math-mode-font-locks))
     (set (make-local-variable 'indent-line-function) 'math-indent-line)
     (set (make-local-variable 'comment-end) " *)")
     (set (make-local-variable 'comment-start) "(* ")
     (set (make-local-variable 'comment-start-skip) "(\\*")
     (set (make-local-variable 'comment-style) 'multi-line)
     (add-hook ',(intern (concat (symbol-name mode-name) "-hook")) 'subword-mode)))

;;;;;;;;;;;
;; Clean up
;;;;;;;;;;;
;; erase the regexps that are quite long
;; note: keywords-regexp and specials-regexp are used interactively
;; and are also not that long. Do not erase them.
(erase-regexp "functions")

(provide 'math-mode-defs)
