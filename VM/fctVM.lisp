(if (consp expr)
    (cond

     ;; macro CLISP dont la macro-expansion peut produire une liste contenant des fonctions ou macros de bas niveau
     ((eq (car expr) 'loop) (metaevalloop (cdr expr) envV envF))
     ((eq (car expr) 'defmacro) (metaevaldefmacro (cdr expr) envV envF))
     ((eq (car expr) 'defun) (metaevaldefun (cdr expr) envV envF ))
     ((eq (car expr) 'defparameter) (metaevaldefparameter (cdr expr) envV envF))
     ((eq (car expr) 'defconstant) (metaevaldefconstant (cdr expr) envV envF ))
     ((eq (car expr) 'defvar) (metaevaldefvar (cdr expr) envV envF))
     ((eq (car expr) 'setf) (metaevalsetf (cdr expr) envV envF ))
     ((eq (car expr) 'setq) (metaevalsetq (cdr expr) envV envF ))
     ((eq (car expr) 'lambda) (metaevallambda (cdr expr) envV envF ))
     ((eq (car expr) 'time) (metaevaltime (cdr expr) envV envF))

     ;; macro CLISP
     ((and (symbolp (car expr)) (macro-function (car expr))) (metamacro expr envV envF))




     ;; forme speciale CLISP
     ((and (symbolp (car expr)) (special-form-p (car expr))) (mevalspecialform expr envV envF))




     ;; fonction ou macro CLISP dont le fonctionnement interne a besoin d'etre modifie pour etre sur que ce soit bien METAEVAL qui evalue jusqu'au bout
     ((eq (car expr) 'member) (metaevalmember (cdr expr) envV envF))
     ((eq (car expr) 'funcall) (metaevalfuncall (cdr expr) envV envF))
     ((eq (car expr) 'apply) (metaevalapply (cdr expr) envV envF ))
     ((eq (car expr) 'mapcar) (metaevalmapcar (cdr expr) envV envF))
     ((eq (car expr) 'eval) (metaevaleval (cdr expr) envV envF))
     ((eq (car expr) 'load) (metaevalload (cdr expr) envV envF))





     ;; lambda-expression
     ((and (consp (car expr)) (eq (caar expr) 'lambda)) (metaapplylambda expr envV envF ))

     ;;  macro METAEVAL
     ((and (symbolp (car expr)) (macrovaleur (car expr) ensvar ensfct)) (metaapplymacro expr envV envF))

     ;;  fonction METAEVAL
     ((and (symbolp (car expr)) (fctvaleur (car expr) ensvar ensfct )) (metaapplyfunction expr envV envF))

     ;; cas par defaut: fonction CLISP
     (t (metafunction expr envV envF))
     )


     *********************************************************


     (defun mevalfunction (expr ensvar ensfct)
     	(if (and (consp (car expr)) (equal 'lambda (caar expr)))
     	    `(lambda ,(cadar expr) (let ,(local ensvar) ,(caddar expr)))
     	    (car expr)))


     (defun metaevallet* (expr ensvar ensfct )
     	(if (atom (car expr))
     	     (metaevalprogn (cdr expr) ensvar ensfct)
     	   (if (atom (caar expr))
     	       (metaevallet* (cons (cdar expr) (cdr expr)) (makeenv (cons (makevv (caar expr) nil) (local ensvar)) (global ensvar)) ensfct)
     	     (metaevallet* (cons (cdar expr) (cdr expr)) (makeenv (addproto (makevv (caaar expr) (meval (cadaar expr) ensvar ensfct)) (local ensvar)) (global ensvar)) ensfct))))

     ; *** traitement de let *** A ENCAPSULER!!!

     (mdefun metaevallet (expr ensvar ensfct )
     	(metaevalprogn (cdr expr) (list (unionenv (reverse (varassoc (car expr) ensvar ensfct)) (local ensvar)) (global ensvar)) ensfct))

     ; *** traitement de quote ***

     (mdefun metaevalquote (expr ensvar ensfct )
     	(car expr))

     ; *** traitement de if ***

     (mdefun metaevalif (expr ensvar ensfct )
     	(if (meval (car expr) ensvar ensfct)
     	    (meval (cadr expr) ensvar ensfct)
     	  (meval (caddr expr) ensvar ensfct )))

     ; *** traitement de progn ***

     (mdefun metaevalprogn (expr ensvar ensfct)
     	(if (null (cdr expr))
     	     (meval (car expr) ensvar ensfct)
     	   (progn (meval (car expr) ensvar ensfct)
     		  (metaevalprogn (cdr expr) ensvar ensfct))))







     (defun mevallabels (expr envV envF)
       (labels ((aux (expr envV envF)
               (if (atom (car expr))
             (mevalprogn (cdr expr) envV envF)
           (aux (cons (cdar expr) (cdr expr)) envV (makeenv (addproto (makefv (caaar expr) (cadaar expr) (cddaar expr) envV envF) (local envF)) (global envF))))))
         (aux (cons (reverse (car expr)) (cdr expr)) envV envF)))

     (defun mevalspecialform (expr envV envF)
     	(cond
     	  ((eq (car expr) 'labels) (mevallabels (cdr expr) envV envF ))
     	  ((eq (car expr) 'function) (mevalfunction (cdr expr) envV envF ))
     	  ((eq (car expr) 'let*) (mevallet* (cdr expr) envV envF ))
     	  ((eq (car expr) 'let) (mevallet (cdr expr) envV envF ))
     	  ((eq (car expr) 'quote) (mevalquote (cdr expr) envV envF ))
     	  ((eq (car expr) 'if) (mevalif (cdr expr) envV envF ))
     	  ((eq (car expr) 'progn) (mevalprogn (cdr expr) envV envF))))
