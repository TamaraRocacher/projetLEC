(require "vm-fct.lisp")
;; Création de la vm
(defun make-vm (vm &optional taille )
  (if (not (null taille))
    (set-stack vm taille)
    (set-stack vm)))

;;Création/définition de la pile (taille par défaut 1000)
(defun set-stack (vm &optional taille )
  (cond
    ((not (null taille))
      (setf (get vm 'pile) (make-array taille :initial-element 0))
      (set-adr vm 3 (- taille 1)) ;;PC
      (set-adr vm 4 (- taille 1)) ;;CC 
      )
    (T
      (setf (get vm 'pile) (make-array 1000 :initial-element 0))
      (set-adr vm 3 999) ;;PC
      (set-adr vm 4 999) ;;CC   
          )
    )
 
  (set-adr vm 0 8) ;BP
  (set-adr vm 1 8) ;FP
  (set-adr vm 2 8) ;SP
 
  
  vm
)
  
  (defun load-asm (vm code)
  (let ((index (get-CC vm)))
    (if
        (atom code)
        (progn 
          (set-adr vm index code)
          (vm-- vm 4)
          (vm++ vm 2)
          )
        (progn 
          (set-adr vm index (car code))
          (vm-- vm 4)
          (vm++ vm 2)
          (load-asm vm (cdr code))
          )
        ))
  (values))
  
  
  
 ;;Lecture et analyse par cas du code pseudo-assembleur (compile)
  (defun read-asm (vm)
  ;;tant que le compteur de prog n'est pas a 0 
  (loop while (not (eq (get-adr vm (aref (get vm 'pile) 3)) 0))
    do
    (let ((op (get-adr vm (aref (get vm 'pile) 3))))
      (if (atom op)
        (return)
        (cond
          ((eq (car op) :stack)
            (empiler vm (+ 2 (cadr op))) ;nombre de parametre + 3 (oSP + oFP + @ ret)
            (set-adr vm 1 (- (get-SP vm) 1))
            (vm-- vm 3)
            )
          
          ((eq (car op) :call)
            (cond
              ((is-operator (cadr op)) (operator vm (cadr op)))
              ((is-lisp-form vm (cadr op)) (lisp-form vm (cadr op)))
            )
            (vm-- vm 3)
            )
          
          ((eq (car op) :const)
            (empiler vm (cadr op))
            (vm-- vm 3)
            )
          
          ((eq (car op) :var)
            (empiler vm (get-adr vm (FP vm '- (+ 1 (- (get-FP-value vm) (cadr op))))))
            (vm-- vm 3)
            )
          
          ((eq (car op) :set-var)
            (set-adr vm (get-adr vm (FP vm '- (+ 1 (- (get-FP-value vm) (cadr op))))) (depiler vm)) 
            (vm-- vm 3)
            )
          
          ((eq (car op) :rtn)
            (let ((val (depiler vm)) (nbpar (depiler vm)) (adret (depiler vm)) (osp (depiler vm)))
              (set-fp vm (- osp 1))
              (set-sp vm (- osp (- nbpar 3)))
              (empiler vm val)
              (set-adr vm 3 (- adret 1)))
            )
          
          ((eq (car op) :skip)
            (vmskip vm (cadr op))
            (vm-- vm 3)
            )
          
          ((eq (car op) :skipnil)
            (vmskipnil vm (cadr op))
            (vm-- vm 3)
            )
          
          ((eq (car op) :skiptrue)
            (vmskiptrue vm (cadr op))
            (vm-- vm 3)
            )
          
          ((eq (car op) :load)
            (empiler vm (get-adr vm (cadr op)))
            (vm-- vm 3)
            )
          ((eq (car op) :store)
            (set-adr vm (cadr op) (depiler vm))
            (vm-- vm 3)
            )))))
  (get-SP-value vm))

;; Affichage de tous les paramètres d'une machine virtuelle.

(defun print-vm (vm)
  (print 'COUNTERS)
  (print (list :BP (get-adr vm 0)))
  (print (list :FP (get-adr vm 1)))
  (print (list :SP (get-adr vm 2)))
  (print (list :PC (get-adr vm 3)))
  (print (list :CC (get-adr vm 4)))
  (print 'PILE) 
  (print (get-pile vm))
  (values)  
  ) 