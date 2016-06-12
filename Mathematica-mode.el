;;
;; A mode to edit Mathematica code in emacs
;;
;; Some ideas and code have been borrowed from
;; Tim Wichmann's mma.el, downloaded the 1st Oct 2011 from
;; http://www2.itwm.fhg.de/as/asemployees/wichmann/mma.html
;; and from Jim Pivarski's mathematica.el downloaded from
;; https://github.com/emacsmirror/mathematica/blob/master/mathematica.el
;; the 1st Oct 2011
;;
;; See math-mode.el and math-interaction-mode.el for the actual code
;; 

(let ((path "~/elisp/math-mode/"))
  (add-to-list 'load-path math-path)
  (load (concat path "math-edit-mode.el"))
  (load (concat path "math-comint-mode.el"))
  (provide 'Mathematica-mode))
