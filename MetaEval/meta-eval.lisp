(require "meval-fct.lisp")

(defun meval (expr &optional env)
  (cond
    ;;Litéral
    ((and (atom expr) (constantp expr))
     expr)
    
    ;;Variable (atome non constant): si environnement défini, on y cherche 
    ;;la valeur assocoiée au symbole expr
    ((atom expr)
     (let ((cell (assoc expr env)))
       (if cell
           (cdr cell)
           (error "~s n'est pas une variable" expr)))
     )
    
    ;; lambda-fonctions
    ((and (consp (car expr)) (eq 'lambda (caar expr)))
     (meval-body(cddar expr)
                 (make-env (cadar expr)
                           (meval-args (cdr expr) env)
                           env))
     
      )
    
    ((or (not (symbolp (car expr))) (constantp (car expr)))
     (error " ~s ne peut être une fonction" (car expr)))
    
    
    ;;Pour les fonctions globales: association d'une valeur fonctionnelle a 
    ;;un nom de fonction
    
    ((get-defun (car expr))
     (let ((fun (get-defun (car expr))))
       (meval-body (cddr fun)
                   (make-env (car (cdr fun))
                             (meval-args (cdr expr) env)
                             ())))
     )
    
    ((eq 'defun (car expr))
     (setf (get-defun (car (cdr expr)))
           `(lambda ,@(cddr expr)))
     )
    
    ((eq 'quote (car expr))
     car (cdr expr))
    
    ;;Structures de contrôle
    ((eq 'if (car expr))
     (if (meval (car (cdr expr)) env)
         (meval (caddr expr) env)
         (meval (cadddr expr)env))
     )
    
    ((not (fboundp (car expr)))
     (error "~s Symbole sans définition fonctionnelle" (car expr)))
    ((special-form-p (car expr))
     (if (null env)
         (eval expr)
         (error " ~s forme spéciale NYI" (car expr))))
    
    
    (t (apply (symbol-function (car expr)) (meval-args (cdr expr) env)))
   )
  )
    
    
(meval '(defun fibo (n) (if (< n 2) n (+ (fibo (- n 1)) (fibo (- n 2))))))