
;;Retourne la pile de la vm (tableau)
(defun get-pile (vm)     
  (get vm 'pile))

 (defun reset (vm)
    (make-vm vm))

(defun reset-pile (vm)
  (set-adr vm 1 8)
  (set-adr vm 2 8))

;;Placer/récupérer la valeur val à l'adresse adr de la pile
(defun set-adr (vm adr val)     
  (setf (aref (get-pile vm) adr) val))

(defun get-adr (vm adr)	
  (aref (get-pile vm) adr))

;revoie le BP
(defun get-BP (vm)
  (aref (get vm 'pile) 0))

;renvoie le FP
(defun get-FP (vm)
  (aref (get vm 'pile) 1))

;renvoie le SP
(defun get-SP (vm)
  (aref (get vm 'pile) 2))
(defun get-SP-value (vm)
  (aref (get vm 'pile) (- (get-SP vm) 1)))

;revoie le PC
(defun get-PC (vm)
  (aref (get vm 'pile) 3))

;renvoie le CC
(defun get-CC (vm)
  (aref (get vm 'pile) 4))

;modifie la valeur du SP
(defun set-SP (vm val)	
  (setf (aref (get vm 'pile) 2) val))

;modifie la valeur du FP
(defun set-FP (vm val)
  (setf (aref (get vm 'pile) 1) val))


(defun empiler (vm x)
  ;(print :stack)
  (vmpush vm (+ 2 x)) ;nombre de parametre + 3 (oSP + oFP + @ ret)
  (set-adr vm 1 (- (get-SP vm) 1))) ;le FP recoit le SP
  
  (defun vm++ (vm adr)
  (let ((v1 (get-adr vm adr)))
    (set-adr vm adr (+ v1 1))))
;;decrement
(defun vm-- (vm adr)
  (let ((v1 (get-adr vm adr)))
    (set-adr vm adr (- v1 1))))