#lang racket
(let ([x 5]) x)
(let ([x 5])
    (let ([x 2]
          [y x])
      (list y x)))

(define b 3)
(define f
  (let ([b b])
    (lambda (x) (* 1 (+ x b)))))
(define g
  ( lambda (x) (+ x b)))
(set! b 2)
(g 3)
(f 3)
(define pr (cons 1 (cons #t "hi"))) ; '(1 #t . "hi")
(define lst (cons 1 (cons #t (cons "hi" null))))
(define hi (cdr (cdr pr)))
(define hi-again (car (cdr (cdr lst))))
(define hi-another (caddr lst))
hi-again
hi-another
(define no (list? pr))
(define yes (pair? pr))
(define of-course (and (list? lst) (pair? lst)))
(define x (cons 14 null))
(define y x)
(set! x (cons 42 null))
(define fourteen (car y))
(define zz (car x))
fourteen
zz


(struct const (int) #:transparent)
const(3)

