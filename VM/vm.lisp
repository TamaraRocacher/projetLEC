(require "fctVM.lisp")

;; Création de la vm
(defun make-vm (vm &optional taille )
  (if (not (null taille))
    (set-pile vm taille)
    (set-pile vm)))https://github.com/LuckyBanana/Compilateur-LISP

;;Création/définition de la pile (taille par défaut 1000)
(defun set-pile (vm &optional taille )
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
 
  ;;Flags 
  ;;(set-adr vm 5 0) ;FLT
  ;;(set-adr vm 6 0) ;FEQ
  ;;(set-adr vm 7 0) ;FGT
  
  ;;(setf (get vm 'etiquettes_resolues) (make-hash-table))
  ;;(setf (get vm 'references_avant) (make-hash-table))
  vm
)
  
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
            (empiler vm (cadr op))
            (vm-- vm 3)
            )
          ((eq (car op) :call)
           
            (vmcall vm (cadr op))
            (vm-- vm 3)
            )
          ((eq (car op) :const)
            (vmconst vm (cadr op))
            (vm-- vm 3)
            )
          ((eq (car op) :var)
            (vmvar vm (cadr op))
            (vm-- vm 3)
            )
          ((eq (car op) :set-var)
            (vmset-var vm) ;(cadr op))
            (vm-- vm 3)
            )
          ((eq (car op) :rtn)
            (vmrtn vm)
            )
          ;;;; !!!!
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
          ((eq (car op) :jump)
            (vmjump vm (cadr op))
            (vm-- vm 3) ; ?
            )
          ((eq (car op) :load)
            (vmload vm (cadr op))
            (vm-- vm 3)
            )
          ((eq (car op) :store)
            (vmstore vm (cadr op))
            (vm-- vm 3)
            )))))
  (get-SP-value vm))

;; Affichage de tous les paramètres d'une machine virtuelle.

(defun print-vm (&optional (nom 'mv))
  (format t "~%Machine virtuelle : ~%--- Nom : ~S ~%--- Taille : ~D" nom (get-taille nom))
  (format t "~%- Registres : ~%--- R0 : ~D ~%--- R1 : ~D ~%--- R2 : ~D ~%--- R3 : ~D"
	  (get-reg nom :R0) (get-reg nom :R1) (get-reg nom :R2) (get-reg nom :R3))
  (format t "~%- Pointeurs : ~%--- BP : ~D ~%--- SP : ~D ~%--- VP : ~D ~%--- FP : ~D"
	  (get-prop nom :BP) (get-prop nom :SP) (get-prop nom :VP) (get-prop nom :FP))
  (format t "~%- Drapeaux : ~%--- DPP : ~D ~%--- DE : ~D ~%--- DPG : ~D"
	  (get-prop nom :DPP) (get-prop nom :DE) (get-prop nom :DPG))
  (format t "~%- Compteurs : ~%--- PC : ~D ~%--- LC : ~D ~%"
	  (get-pc nom) (get-lc nom))
  )
