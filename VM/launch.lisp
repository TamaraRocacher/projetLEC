(require "vm.lisp")
(make-vm 'vm)
;(load-code 'vm (compilation '(+ 1 2)))
(load-asm 'vm '((:CONST 1) (:CONST 2) (:CALL +)))

(read-asm 'vm)