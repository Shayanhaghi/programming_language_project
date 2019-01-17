#lang racket
(struct const(int) #:transparent)
(define (eval-exp e)
  (cond [(const? e) e]
        )
 )

(eval-exp (const 3))
(const 17)
(const (const 3))
(struct card(suit rank) #:transparent #:mutable)

(define x (card 3 4))
(set-card-suit! x 10)
x