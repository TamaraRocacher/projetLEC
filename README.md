# projetLEC
compilateur et interpréteur LISP

Pour le Méta-évaluateur :
(load "meta-eval.lisp")
(meval '(fibo 15)) ; Devrait fonctionner
(eval '(fibo 15)) ;Devrait ne pas fonctionner car fibo a été définit dans l'environnement de notre meta-evaluateur mais pas dans clisp


Pour la VM à pile :
(load "launch.lisp")
(load-asm 'vm '(CODE_ASM))
(read-asm 'vm)

Nous n'avons ni la traduction de lisp au langage intermédiaire ni celle du langage intermédiaire vers asm.