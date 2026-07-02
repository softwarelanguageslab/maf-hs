#lang simpleactor
;; This example program test the "self^" functionality of the SimpleActor language.
(letrec
  ((a (lambda () 
        (receive 
          ((sender (send^ sender 'reply)
                   (a))))))
   (b (lambda ()
        (parametrize 
          ((self (self^)))
                (receive
                  (((cons 'start a) (send^ a (dyn self)) (b))
                   ('reply (trace 'done) (b)))))))
   (a-actor (spawn^ (a)))
   (b-actor (spawn^ (b))))

  (send^ b-actor (cons 'start a-actor)))
