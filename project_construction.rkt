;; PL Project - Fall 2018
;; NUMEX interpreter
#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for NUMEX programs


;; type of string, int, boolean should be varified
;; boolean -> #t, #f
;; int -> check int -> integer?, boolean? e.g., (boolean? #f)
(struct var  (string) #:transparent)    ;; a variable, e.g., (var "foo")
(struct num  (int)    #:transparent)    ;; a constant number, e.g., (num 17)
(struct bool (boolean) #:transparent)   ;; a boolean value, e.g., (boo #t or #f)


;; all structs should have (num x) input, this should be implemented and varified.

(struct plus   (e1 e2)  #:transparent)          ;; add two expressions (plus (num 4)(num 1))
(struct minus  (e1 e2)  #:transparent)          ;; subtraction, e.g., (minus (num 4) (num 2)) = (num 2)
(struct mult   (e1 e2)  #:transparent)          ;; multiplication, e.g, (mult (num 10) (num 3)) = (num 30)
(struct div    (e1 e2)  #:transparent)          ;; dividing, e.g, (div (num 3)(num 2)) = (num 1) ;;semantic is challenge -> division by zero should be handled.
(struct neg    (e1)     #:transparent)          ;; negation, e.g, (neg (num 2)) = (num -2)


;; e1, e2 should have a boolean value, this should be implemented and varified.
;; does lazyness imply even if syntaxt is not valid we should calculate the result? 
(struct andalso (e1 e2)   #:transparent)          ;; andalso, e.g (andalso (bool #t)(bool #f)) = bool #f   -> should be lazy 
(struct orelse (e1 e2)    #:transparent)          ;; orles, e.g   (orelse  (bool #t)(bool #f)) = bool #t   -> should be lazy


;; condition should be boolean and e2, e3 should be any thing
;; impelemenation should be lazy, again does this imply lack of checking validity of e3 as a sound syntaxt
;; so could we not evaluate expression and be sure of not commiting type error ?
(struct cnd (condition e2 e3) #:transparent)      ;; if condition is true then result equals e2 else result is e3
                                                ;; e.g. (cnd (bool #t) e2 e3) -> e2
                                                ;; e.g. (cnd (bool #f) e2 e3) -> e3
                                                ;; other test case (cnd (bool #t) e2 e3) -> e2
                                                ;; other test case (cnd (num  #t) e2 e3) -> e2



;input could be (var string), (num int), (bool boolean)
; but both should have the same type and equality should be defined for those for every type
; question(?) what about other data type ?
;   answer in page 3 of 5 in semantic of iseq which we should just write this function for bool and num
(struct iseq (e1 e2) #:transparent)   ;;  test case (iseq  (bool #t) (bool #f)) -> (bool #f)
                                      ;;  test case (iseq  (num 3 ) (num 4)) -> (bool #f)


(struct ifnzero (e1 e2 e3) #:transparent)    ;;  test case (ifnzero (num 0) e2 e3) -> e2
                                            ;;  test case (ifnzero (num 1) e2 e3) -> e3
                                            ;;  test case (ifnzero (bool #t) e2 e3) -> error
                                            ;;  error : boolean is passed as a first argument which should be a number.




; the evaluation of e3 or e4 should be lazy 
(struct ifleq (e1 e2 e3 e4) #:transparent)  ;;  lower or equal, e.g. (ifleq (num 3)(num 4) e3 e4 ) -> e3 
                                            ;;  lower or equal, e.g. (ifleq (num 4)(num 4) e3 e4) -> e3
                                            ;;  lower or equal, e.g. (ifleq (num 5)(num 4) e3 e4) -> e4
                                            ;;  lower or equal, e.g. (ifleq (bool 5)(num 4) e3 e4) -> error
                                            ;;  lower or equal, e.g. 



; s1 can be null, is it our null or meta language null ?
; 
(struct lam (s1 s2 e) #:transparent)        ;;  In e, s1 is bound to the function itself(for recursion)
                                            ;;  s2 is bound to the only argument
                                            ;;  result of evaluating a function is closure


(struct apply (e1 e2) #:transparent)        ;; apply  semantic is a complicated one


(struct with (s e1 e2) #:transparent)       ;; where the value of e1  is bound to s in e2

; constructing pair but should e1 and e2 be the same values ?
(struct apair (e1 e2) #:transparent)        ;; what do we have about semantic of a pair?
                                            ;; what should happens in semantic of env ?
                                            ;; simply we can return the value of (apair f(e1) f(e2))
(struct 1st (e1) #:transparent)             ;; returns the first part of a pair (1st e1) -> if e1 == pair -> e2 else error

(struct 2nd (e1) #:transparent)             ;; the second part of a pair (2nd e1) -> if e1 == pair -> e2 else error
      

; this struct should be checked !!
(struct munit () #:transparent)              ;;

(struct ismunit (e1) #:transparent)          ;; check if it is munit or not 

(struct closure (env f) #:transparent)       ;;

(ismunit 3)
;; Problem 1

(define (racketlist->numexlist xs)
  (cond [(not(list? xs)) (error "not a list")]
        [(null? (cdr xs))(apair (car xs) (munit))]
        [else (apair (car xs)(racketlist->numexlist (cdr xs)))]
        )
  )



(define (numexlist->racketlist xs)
  (cond
        [(not(apair? xs)) (error "input is not a list") ]
        [(equal? (apair-e2 xs)(munit)) (cons (apair-e1 xs) null)]
        [else (cons (apair-e1 xs) (numexlist->racketlist (apair-e2 xs)))]
  )
)
;; test cases ::
(define result1 (racketlist->numexlist (list 1 2 4)))
result1
(define result2 (numexlist->racketlist result1))
(define x (apair 2 (munit)))
result2


;; Problem 2

;; part1 -> envlookup function
;; lookup a variable in an environment
;; Complete this function
;; it is considered that we have list of (cons a b)
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable" str "during evaluation in NUMEX Language")]
          [(equal? (car(car env)) str) (cdr(car env))]
          [else (envlookup (cdr env) str)]
          )
		
 )
;; test cases for env
(define test_env (list (cons "ab" 23) (cons "cd" 34)))
(envlookup test_env "c")


;; problem 2
;; part2 -> interpreter









