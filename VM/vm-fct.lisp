
;;Retourne la pile de la vm (tableau)
(defun get-pile (vm)     
  (get vm 'pile))

(defun reset-pile (vm)
  (set-adr vm 1 8)
  (set-adr vm 2 8))

;;Placer/récupérer la valeur val à l'adresse adr de la pile
(defun set-adr (vm adr val)     
  (setf (aref (get-pile vm) adr) val))

(defun get-adr (vm adr)	
  (aref (get-pile vm) adr))

;revoie le BP (Base Pointer)
;(defun get-BP (vm)
 ; (aref (get vm 'pile) 0))

;renvoie le FP (Frame Pointer)
(defun get-FP (vm)
  (aref (get vm 'pile) 1))

;renvoie le SP (Stack Pointer)
(defun get-SP (vm)
  (aref (get vm 'pile) 2))
(defun get-SP-value (vm)
  (aref (get vm 'pile) (- (get-SP vm) 1)))

;revoie le PC (Program Counter)
;(defun get-PC (vm)
 ; (aref (get vm 'pile) 3))

;renvoie le CC (Code Counter)
(defun get-CC (vm)
  (aref (get vm 'pile) 4))

;modifie la valeur du SP
;(defun set-SP (vm val)	
 ; (setf (aref (get vm 'pile) 2) val))

;modifie la valeur du FP
(defun set-FP (vm val)
  (setf (aref (get vm 'pile) 1) val))

(defun SP (vm op index)
  (cond 
    ((eq '- op)
     (- (get-SP vm) index))
    ((eq '+ op)   
     (+ (get-SP vm) index))))

;renvoie la valeur du FP +/- index
(defun FP (vm op index)
  (cond 
    ((eq '- op)
     (- (get-FP vm) index))
    ((eq '+ op)   
     (+ (get-FP vm) index))))

(defun get-SP-value (vm)
  (aref (get vm 'pile) (- (get-SP vm) 1)))

(defun get-FP-value (vm)
  (get-adr vm (get-FP vm)))



;; incrémenter valeur à l'adresse adr
  (defun vm++ (vm adr)
  (let ((v1 (get-adr vm adr)))
    (set-adr vm adr (+ v1 1))))
;;decrementer valeur à l'adresse adr
(defun vm-- (vm adr)
  (let ((v1 (get-adr vm adr)))
    (set-adr vm adr (- v1 1))))


(defun empiler (vm val)
  (set-adr vm (get-SP vm) val)
  (vm++ vm 2) 
  ) 

(defun depiler (vm)
  (progn  
    (vm-- vm 2)
    (get-adr vm (get-SP vm)))
  ) 



(defun is-operator (fct) 
  (or
    (eq '+ fct)
    (eq '- fct)
    (eq '* fct)
    (eq '/ fct)
    (eq '++ fct)
    (eq '-- fct)
    (eq '> fct)
    (eq '< fct)
    (eq '= fct)
    (eq '<= fct)
    (eq '>= fct)))

(defun operator (vm fct)
  (let ((src (SP vm '- 1)) (dest (SP vm '- 2)))
    (cond 
      ((eq '+ fct)
        (let ((v2 (depiler vm)) (v1 (depiler vm)))
              (empiler vm (+ v1 v2))))
      ((eq '- fct)  
        (let ((v2 (depiler vm)) (v1 (depiler vm)))
            (empiler vm (- v1 v2))))
      ((eq '* fct)
        (let ((v2 (depiler vm)) (v1 (depiler vm)))
            (empiler vm (* v1 v2))))
      ((eq '/ fct)
        (let ((v2 (depiler vm)) (v1 (depiler vm)))
            (empiler vm (/ v1 v2))))
      ((eq '++ fct)
          (vm++ vm src))
      ((eq '-- fct)
        (vm-- vm src))
      ((eq '= fct)
       (let ((v2 (depiler vm)) (v1 (depiler vm)))
         (empiler vm (= v1 v2))))
      ((eq '> fct)
       (let ((v2 (depiler vm)) (v1 (depiler vm)))
         (empiler vm (> v1 v2))))
      ((eq '< fct)
       (let ((v2 (depiler vm)) (v1 (depiler vm)))
         (empiler vm (< v1 v2))))
      ((eq '<= fct)
       (let ((v2 (depiler vm)) (v1 (depiler vm)))
         (empiler vm (<= v1 v2))))
      ((eq '>= fct)
       (let ((v2 (depiler vm)) (v1 (depiler vm)))
         (empiler vm (>= v1 v2))))
      )))

(defun is-lisp-form (vm op)
  (or
    (eq 'print op)
    (eq 'get op)
    (eq 'make-hash-table op)
    (eq 'atom op)
    (eq 'first op)
    (eq 'second op)
    (eq 'cdr op)
    (eq 'eq op)
    (eq 'symbolp op)
    (eq 'aref op)
    (eq 'get-cc op)
    (eq 'set-adr op)
    (eq 'get-pile op)
    (eq 'vm-- op)
    (eq 'get-adr op)
  )
  )

(defun lisp-form (vm fct)

  (case fct
    ('print
      (print (depiler vm)))
    ('get
      (let ((arg2 (depiler vm)) (arg1 (depiler vm)))
        (empiler vm (get arg1 arg2))))
    ('make-hash-table
      (empiler vm (make-hash-table)))
    ('atom
      (empiler vm (atom (depiler vm))))
    ('first
      (empiler vm (first (depiler vm))))
    ('second
      (empiler vm (second (depiler vm))))
    ('cdr
      (empiler vm (cdr (depiler vm))))
    ('eq
      (empiler vm (eq (depiler vm) (depiler vm))))
    ('symbolp
      (empiler vm (symbolp (depiler vm))))
    ('aref
      (let ((arg2 (depiler vm)) (arg1 (depiler vm)))
        (empiler vm (aref arg1 arg2))))
    ('get-cc
      (empiler vm (get-cc (depiler vm))))
    ('set-adr
      (let ((arg3 (depiler vm)) (arg2 (depiler vm)) (arg1 (depiler vm)))
        (empiler vm (set-adr arg1 arg2 arg3))))
    ('get-pile
      (empiler vm (get-pile (depiler vm))))
    ('vm--
      (let ((arg2 (depiler vm)) (arg1 (depiler vm)))
        (empiler vm (vm-- arg1 arg2))))
    ('get-adr
      (let ((arg2 (depiler vm)) (arg1 (depiler vm)))
      (empiler vm (get-adr arg1 arg2))))
        ))

;;**************************
;;********A FINIR***********
;;**************************


(defun vmrtn (vm)
  
  (let ((val (depiler vm)) (nbpar (depiler vm)) (adret (depiler vm)) (osp (depiler vm)))
    (set-fp vm (- osp 1))
    (set-sp vm (- osp (- nbpar 3)))
    (empiler vm val)
    ;(set-adr vm (get-SP vm) val)
    (set-adr vm 3 (- adret 1))))
 
  

(defun vmskip (vm n)
  ;(print :skip)
  (set-adr vm 3 (- (get-PC vm) n)))

(defun vmskipnil (vm n)
  ;(print :skipnil)
  ;(print "---SP---Value---")
  ;(print (get-SP-value vm))
  (let ((val (depiler vm)))
    ;(print val)
    (if (eq nil val) ;(get-SP-value vm))
      (vmskip vm n)
      ()
      )))

(defun vmskiptrue (vm n)
  ;(print "---SP---Value---")
  ;(print (get-SP-value vm))
  (let ((val (depiler vm)))
  (if (eq T val)
    (vmskip vm n)
    ()
    )))







  
