(require "fctMEval.lisp")


;;Lancement du metaevaluateur
(defun meval (expr &optional envV envF)

    ;;si expr est une liste
    (if (consp expr)

      (cond
      ;; macros LISP
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
       ((and (symbolp (car expr)) (macro-function (car expr))) (metamacro expr ensvar ensfct))

      ((eq (car expr) 'eval) (mevaleval (cdr expr) envV envF))
      ;; liste de 1er elemt = 'lambda
      ((and (consp (car expr)) (eq (caar expr) 'lambda)) (metaapplylambda expr envV envF ))
      ((null envV)(eval expr))
      (t (metafunction expr envV envF))
      )

      ;; sinon si expr est un keyword
	     (if (keywordp expr)
	         expr
         ;;sinon si expr est un symbole
	        (if (symbolp expr)
		        (metasymbol  expr envV envF )
	         expr))))
