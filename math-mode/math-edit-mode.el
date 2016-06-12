;;
;; A mode to edit Mathematica code in emacs
;;
;; math-edit-mode
;; Basically derives everything from
;; math-mode-definitions and sets up the basic mode to edit
;; Mathematica code.

(require 'math-mode-defs)

(defvar math-edit-mode-syntax-table
   math-mode-syntax-table)
(defvar math-edit-mode-map nil
  "Keymap for Mathematica major mode.")

(when (not math-edit-mode-map)
  (setq math-edit-mode-map
	(let ((map (make-sparse-keymap)))
	  (math-make-keys map)
	  map)))

(define-derived-mode math-edit-mode fundamental-mode "Mathematica:edit"
  "A mode to edit Mathematica code in Emacs. Provides a simple
and lisp-inspired indentation scheme, syntax highlighting and
some custom commands."
  (math-make-mode math-edit-mode))


(provide 'math-edit-mode)
