;; 
;; my-fav-files.el
;; 
;; An Emacs command for quick access to a list of user defined
;; favorite files.
;; 

;;;;;;;;;;;;
;; Variables
;;;;;;;;;;;;
(defvar mff-alist '(("." . "~/.emacs"))
  "An alist each of whose elements are of the form (binding
. path). binding is the user defined binding for visiting the
file at path. Both binding and path are strings, and binding must
be a understandable by define-key.")

(defvar mff-buffer-name "*Favorite Files*"
  "Name of the buffer in which to show favorite files. See
  mff-show-list.")

(defvar mff-mode-map nil
  "Keymap tying every favorite file to its user defined
keybinding. Do not modify this keymap directly.")

;;;;;;;;;;;
;; Commands
;;;;;;;;;;;
(defun mff-show-list ()
  "Show a buffer with a list of favorite files and their
bindings. Press the defined binding to immediately visit the
corresponding file."
  (interactive)
  (switch-to-buffer mff-buffer-name)
  (toggle-read-only -1)
  (erase-buffer)
  (insert (format " %s\t%s\t\n ---\t----\n" "KEY" "FILE"))
  (mapcar (lambda (assoc)
	    (insert (format " %3s\t%s\n" (car assoc) (cdr assoc))))
	  mff-alist)
  (toggle-read-only 1)
  (mff-mode))

(defun mff-add-binding (key file)
  "Add a single new file to the favorite files list. When called
interactively, prompt for binding and file path."
  (interactive "MKey: \nMFile: ")
  (eval `(mff-add-files ((,key . ,file))))
  (if (string= (buffer-name) mff-buffer-name)
      (mff-show-list)))

(defmacro mff-add-files (files)
  "Add files to the favorite files list."
  `(progn
     ,@(mapcar
	(lambda (assoc)
	  `(progn
	     (add-to-list 'mff-alist '(,(eval `,(car assoc)) . ,(eval `,(cdr assoc))))
	     (define-key mff-mode-map ,(car assoc)
	       (lambda ()
		 (interactive)
		 (find-file ,(eval `,(cdr assoc)))))
	     (define-key mff-mode-map ,(concat "4" (car assoc))
	       (lambda ()
		 (interactive)
		 (find-file-other-window ,(cdr assoc))))))
	files)))

;;;;;;;;;;;
;; Coloring
;;;;;;;;;;;
(defvar mff-mode-font-locks
  `(("/?\\([^/]*?\.el\\)$" 1 font-lock-function-name-face)
    ("/?\\([^/]*?\.org\\)$" 1 font-lock-comment-face)
    ("/?\\([^/]*?\.lua\\)$" 1 font-lock-keyword-face))
  "Syntax highlighting for Mathematica mode.")

;;;;;;;;;;;;;;
;; Keybindings
;;;;;;;;;;;;;;
(when (not mff-mode-map)
  (setq mff-mode-map
	(let ((map (make-sparse-keymap)))
	  (define-key map (kbd "TAB") 'mff-show-list)
	  (define-key map (kbd "+") 'mff-add-binding)
	  (define-key map (kbd "q")
	    (lambda ()
	      (interactive)
	      (if (string= (buffer-name) mff-buffer-name) (bury-buffer))))
	  map)))

(define-derived-mode mff-mode fundamental-mode "Favorite Files"
  "Dummy mode for the TAB key, which is bound to mff-show-list."
  (set (make-local-variable 'font-lock-defaults) '(mff-mode-font-locks)))


(provide 'mff)
