;;macro setf-able permettant d'acceder la fonction associée a un symbole
(defmacro get-defun (symb) 
`(get ,symb :defun))

;; Création de la liste d'associations param/valeur
;;contenant les éléments de env

;;******
;;(if (eql (car lparam) '&rest)
;;	(let ((l (make-env-aux (cadr lparam) lval)))
;;	  (if (eql (length lparam) (+ 1 (length lval)))
;;	      (cons (cons (car l) (cdr l))env)
;;	    (cons (cons (car l) (cdr l)) env) 
;;
;;	    ))
;;**********

(defun make-env (lparam lval &optional env)
  (if (null lparam) 
      env
    	(if (eql (car lparam) '&optional)
	  		(if (null (cddr lparam))
	      		(cons (cons (cadr lparam) (car lval)) (make-env  (cddr lparam) (cdr lval) env))
	    		(cons (cons (cadr lparam) (car lval)) (make-env (cons '&optional (cddr lparam)) (cdr lval) env))
	    	)
			(if (not lval)
	    		env
				(cons (cons (car lparam) (car lval)) (make-env (cdr lparam) (cdr lval) env))
    		)
   		)
    )
)

;;Retourne la liste de valeurs obtenue a partir d'une liste d'expr et un env
(defun meval-args (lexpr &optional env)
  (if ( eq lexpr nil)
      	nil
    	(if ( eq (cdr lexpr) nil)
			(cons (meval (car lexpr) env) nil)
      		(cons (meval (car lexpr) env) (meval-args (cdr lexpr) env))
       	)
    )
)

;;Evalue chaque expr de la liste lexpr puis
;;Retourne la valeur rendue par la dernière evaluation 
(defun meval-body (lexpr &optional env)
  (if ( eq lexpr nil)
      nil
      (if ( eq (cdr lexpr) nil)
	      (meval (car lexpr) env)
          (progn (meval (car lexpr) env) (meval-body (cdr le) env))
        )
    )
)