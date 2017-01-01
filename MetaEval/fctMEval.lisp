(defun metaargs (expr envV envF)
	(if (null expr)
	     nil
	   (cons (meval (car expr) envV envF) (metaargs (cdr expr) envV envF))))


(defun quotelst (lst)
  	(if (consp lst)
  	    (cons (list 'quote (car lst)) (quotelst (cdr lst)))))

(defun metafunction (expr envV envF)
      (eval `(,(car expr) ,@(quotelst (metaargs (cdr expr) envV envF)))))



(defun cstvaleur (expr envV envF)
      	(let ((r (varvaleur expr envV envF)))
      	  (if (eq (caddr r) 'const)
      	      r)))
(defun varvaleur (expr envV envF )
 	(let ((r (assoc expr (car envV))))
				(if r
						r
					(assoc expr (cadr envV)))))

(defun mevaleval (expr envV envF )
    (meval (meval (car expr) envV envF ) envV envF))


(defun metasymbol (expr envV envF )
	(let ((r (cstvaleur  expr envV envF))
	      (v (varvaleur  expr envV envF)))
	  (if r
	      (eval r)
	    (if v
		(eval v)
	      (eval  expr)))))
