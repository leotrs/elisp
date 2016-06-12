;;
;; A mode to edit Mathematica code in emacs
;;
;; math-comint-mode.el
;; Builds a Mathematica interpreter for Emacs on top of comint mode.

(require 'math-mode-defs)

;;;;;;;;;;;;
;; Variables
;;;;;;;;;;;;
(defvar math-kernel-command "math"
  "Shell command to run a Mathematica kernel.")
(defvar math-comint-buffer nil
  "Comint kernel buffer.")
(defvar math-comint-query-on-exit nil
  "By default, do not query to save the buffer on exit.")

(defvar math-comint-mode-syntax-table
  math-mode-syntax-table)

;;;;;;;;;;;;;;;;;;;;
;; Commands & keymap
;;;;;;;;;;;;;;;;;;;;
(defun math-comint-start (arg)
  "Start a Mathematica kernel. If there's a prefix arg, open it in another window."
  (interactive "p")
  (when (not math-comint-buffer)
    (setq math-comint-buffer (make-comint "Mathematica Interpreter" math-kernel-command))
    (with-current-buffer math-comint-buffer
      (math-comint-mode)))
  (if arg
      (switch-to-buffer-other-window math-comint-buffer)
    (switch-to-buffer math-comint-buffer)))

(defvar math-comint-mode-map nil
  "Keymap for Mathematica major mode.")
;; check to avoid clashes, e.g., by loading the package twice
(when (not math-comint-mode-map)
  (setq math-comint-mode-map
	(let ((map (make-sparse-keymap)))
	  (set-keymap-parent map comint-mode-map)	  
	  (math-make-keys map)
	  map)))

;;;;;;;;;;;;;;;;;;
;; Mode definition
;;;;;;;;;;;;;;;;;;
(define-derived-mode math-comint-mode comint-mode "Mathematica Comint"
  "A mode to interact with a Mathematica command line kernel
through Emacs. Built on top of comint. Use math-comint-start to
start the Mathematica interpreter in a comint buffer. Calling
math-comint-mode will only set up comint-mode but will not start
a kernel."
  (math-make-mode math-comint-mode)
  (set-process-query-on-exit-flag (get-buffer-process math-comint-buffer)
				  math-comint-query-on-exit))


(provide 'math-comint-mode)
