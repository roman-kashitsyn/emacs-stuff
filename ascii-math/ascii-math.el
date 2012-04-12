;;;; Library for funny ascii math formatting

(require 'cl)

;;; Lets begin with small set of functions to construct text elements

(defstruct element
  "ASCII graphic element."
  content width height)

(setq *space* (aref " " 0))

(defun -mk-2d-array (w h elem)
  (make-vector h (make-vector w elem)))

(defun -join-2d-arrays-hor (l r)
  (mapcar* 'vconcat l r))

(defun -join-2d-arrays-ver (top bot)
  (vconcat top bot))

(defun char-elem (ch w h)
  (make-element
   :content (-mk-2d-array w h ch)
   :width w
   :height h))

(defun string-elem (str)
  (make-element :content (make-vector 1 str)
		:width (length str)
		:height 1))

(defun concat-horizontally (left right)
  (make-element
   :content (-join-2d-arrays-hor (element-content left)
				 (element-content right))
   :width (+ (element-width left) (element-width right))
   :height (max (element-height left) (element-height right))))

(defun concat-vertically (top bot)
  (make-element
   :content (-join-2d-arrays-ver (element-content top)
				 (element-content bot))
   :width (max (element-width top) (element-width bot))
   :height (+ (element-height top) (element-height bot))))

(defun elem-above (left right)
  (let* ((left1  (elem-widen left (element-width right)))
	 (right1 (elem-widen right (element-width left))))
    (concat-vertically left1 right1)))

(defun elements-above (&rest element-list)
  (reduce 'elem-above element-list))

(defun elem-beside (top bot)
  (let* ((top1 (elem-heighten top (element-height bot)))
	 (bot1 (elem-heighten bot (element-height top))))
    (concat-horizontally top1 bot1)))

(defun elements-beside (&rest element-list)
  (reduce 'elem-beside element-list))

(defun elem-widen (elem width)
  (let ((w (element-width elem))
	(h (element-height elem)))
    (if (<= width w)
	elem
      (let* ((left-width (/ (- width w) 2))
	     (left  (char-elem *space* left-width h))
	     (right (char-elem *space* (- width w left-width) h)))
	(elements-beside left elem right)))))

(defun elem-heighten (elem height)
  (let ((w (element-width elem))
	(h (element-height elem)))
    (if (<= height h)
	elem
      (let* ((top-height (/ (- height h) 2))
	     (top (char-elem *space* w top-height))
	     (bot (char-elem *space* w (- height h top-height))))
	(elements-above top elem bot)))))

(defun elem->string (elem)
  (mapconcat (lambda (e) e) (vconcat (element-content elem)) "\n"))

;;; Ok, here the math begins
(setf *op-groups*
      '((| ||)
	(& &&)
	(^)
	(== !=)
	(< <= > >=)
	(+ -)
	(* %)))

(defun -make-precedence (groups)
  (let ((table (make-hash-table))
	(index 0))
    (dolist (ops groups)
      (dolist (op ops)
	(puthash op index table))
      (incf index))
    table))

(setf *fraction-precedence* -1)
(setf *precedence* (-make-precedence *op-groups*))
(setf *unary-precedence* (length op-groups))

(defun format-expr (expr)
  (elem->string (-format-expr expr 0)))

(defun -format-expr (expr encl-prec)
  (if (atom expr)
      (string-elem (format "%s" expr))
    (let ((op (car expr)))
      (cond
       ((eq op '/)
	(-format-fraction (elt expr 1) (elt expr 2) encl-prec))
       ;; unary operator
       ((= 2 (length expr))
	(elem-beside (string-elem (format "%s" op))
		     (-format-expr (elt expr 1) *unary-precedence*)))
       ;; known binary operator
       ((gethash op *precedence* nil)
	(-format-op op (elt expr 1) (elt expr 2) encl-prec))
       (t (error (format "Can not format expression %s" expr)))))))

(defun -format-fraction (left-expr right-expr encl-prec)
  (let* ((top (-format-expr left-expr *fraction-precedence*))
	 (bot (-format-expr right-expr *fraction-precedence*))
	 (line (char-elem ?- (max (element-width top)
				  (element-width bot)) 1))
	 (frac (elements-above top line bot)))
    (if (/= encl-prec *fraction-precedence*)
	frac
      (elements-beside (string-elem " ") frac (string-elem " ")))))

(defun -format-op (op left-expr right-expr encl-prec)
  (let* ((op-prec (gethash op *precedence* 0))
	 (left (-format-expr left-expr op-prec))
	 (right (-format-expr right-expr (+ 1 op-prec)))
	 (oper (elements-beside left
				(string-elem (format " %s " op))
				right)))
    (if (<= encl-prec op-prec)
	oper
      (elements-beside (string-elem "(")
		       oper
		       (string-elem ")")))))

(defun get-last-sexpr ()
  (save-excursion
    (save-restriction
      (backward-sexp)
      (mark-sexp)
      (buffer-substring (mark) (point)))))

(defun format-last-sexp ()
  "Formats last expression."
  (interactive)
  (next-line)
  (insert-string (format-expr (car (read-from-string (get-last-sexpr))))))


;; (format-expr '(/ 125 138))
;; (format-expr '(+ 1 2))
;; (format-expr '(/ (+ a b) (- a b)))
;; (format-expr '(/ (/ a b) c))
;; (format-expr '(/ (* 2 (+ 1 2)) (+ 12 18)))

;; (setf square-3x3 (char-elem ?- 3 3))
;; (elem->string square-3x3)
;; (elem-widen square-3x3 10)
;; (elem->string (elem-beside (string-elem "abcd") square-3x3))
;; (elem->string (elem-above (string-elem "abcd") (string-elem "ef")))
;; (elem->string (elements-above (string-elem "abcd") (string-elem "hello") (string-elem "no")))

