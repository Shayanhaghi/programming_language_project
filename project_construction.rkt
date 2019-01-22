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
;; append string and corresponding value to the environment
;; if string is not valid it doesn't append and raise error.
(define (env_append env string_inp value)
  (cond [(not (string? string_inp)) (error "first argument should be a string")] 
        [else (append (list (cons string_inp value)) env)])
  )

;; test cases for env
;(define test_env (list (cons "ab" 23) (cons "cd" 34)))
;(envlookup test_env "ab")
;(envlookup (append (list (cons "cd" 35)) test_env) "cd")
;(envlookup (env_append null "cd" (num 3)) "j")


;; problem 2
;; part2 -> interpreter

;; Complete more cases for other kinds of NUMEX expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond
       
         [(var? e) 
         (envlookup env (var-string e))]
     
        [(plus? e) 
         (let ([v1 (eval-under-env (plus-e1 e) env)]
               [v2 (eval-under-env (plus-e2 e) env)])
           (if (and (num? v1)
                    (num? v2))
               (num (+ (num-int v1) 
                       (num-int v2)))
               (error "NUMEX addition applied to non-number")))]

        [(num? e)
          (cond [(integer? (num-int e)) e]
                [else (error "not a valid integer check syntax of int for more information :")]
           )
          ]

         [(bool? e)
          (cond [(boolean? (bool-boolean e)) e]
                [else (error "not a valid boolean #t or #f are valid ones")]
                )
          ]
         [(minus? e)
          (let ([v1 (eval-under-env (minus-e1 e) env)]
                [v2 (eval-under-env (minus-e2 e) env)])
                (if (and (num? v1) (num? v2))
                  (num (- (num-int v1) 
                          (num-int v2)))
                  (error "NUMEX subtraction applied to non-number")))
          ]
         [(mult? e)
          (let ([v1 (eval-under-env (mult-e1 e) env)]
                [v2 (eval-under-env (mult-e2 e) env)])
                (if (and (num? v1) (num? v2))
                  (num (* (num-int v1) 
                          (num-int v2)))
                  (error "NUMEX multiplication applied to non-number")))
          ]
         [(div? e)
          (let ([v1 (eval-under-env (div-e1 e) env)]
                [v2 (eval-under-env (div-e2 e) env)])
                (if (and (num? v1) (num? v2))
                    (cond[(equal? (num-int v2) 0) (error "division by zero in numex")]
                         [else  (num (/ (num-int v1) 
                         (num-int v2)))])
                  (error "NUMEX division applied to non-number")))
          ]
         [(neg? e)
          (let ([v1 (eval-under-env (neg-e1 e) env)])
            (cond [(bool? v1)
                   (cond [(equal? (bool-boolean v1) #t) (bool #f)]
                         [(equal? (bool-boolean v1) #f) (bool #t)])
                  ]
                  [(num? v1)
                   (num (-(num-int v1)))
                  ]
                  [else (error "negation applied on data other than bool or num")
                   ]
           )
          )
          ]
         [(andalso? e)
           (let ([v1 (eval-under-env (andalso-e1 e) env)]
                [v2 (eval-under-env (andalso-e2 e) env)])
                (cond [(and (bool? v1) (bool? v2))
                       (cond [(equal? (bool-boolean v1) #f) (bool #f)]
                             [else (cond
                                     [(equal? (bool-boolean v2) #t) (bool #t)]
                                     [else (bool #f)])])
                       ]
                      [else (error "andalso requires two boolean exactly.")]
                      )
             )
           ]

           [(orelse? e)
             (let ([v1 (eval-under-env (orelse-e1 e) env)]
                [v2 (eval-under-env (orelse-e2 e) env)])
                (cond [(and (bool? v1) (bool? v2))
                       (cond [(equal? (bool-boolean v1) #t) (bool #t)]
                             [else (cond
                                     [(equal? (bool-boolean v2) #t) (bool #t)]
                                     [else (bool #f)])])
                       ]
                      [else (error "orelse requires two boolean exactly.")]
                      )
                
             )
            ]
           [(cnd? e)
            (let ([condition (eval-under-env (cnd-condition e) env)])
              (cond [(bool? condition)
                     (cond [(equal? (bool-boolean condition) #t)
                            (eval-under-env (cnd-e2 e) env)]
                           [else
                            (eval-under-env (cnd-e3 e) env)]

                           )]
                    [else (error "In Cnd first element should be a boolean ")]
                )

             )
            ]
           [(iseq? e)
            (let ([v1 (eval-under-env (iseq-e1 e) env)]
                  [v2 (eval-under-env (iseq-e2 e) env)]
                  )
              (cond [(equal? (and (bool? v1) (bool? v2)) #t)
                     (cond [(equal? (bool-boolean v1) (bool-boolean v2)) (bool #t)]
                           [else (bool #f)])]
                    [(equal? (and (num? v1)(num? v2)) #t)
                     (cond [(equal? (num-int v1) (num-int v2)) (bool #t)]
                           [else (bool #f)])]
                    [else (error "both of the arguments for iseq should be either bool or num.")])
              )
            ]
           [(ifnzero? e)
            (let ([number (eval-under-env (ifnzero-e1 e) env)])
              (cond
               [(num? number)
                (cond
                  [(equal? (num-int number) 0) (eval-under-env (ifnzero-e3 e)  env)]
                  [else (eval-under-env (ifnzero-e2 e)  env)]
                  )
                ]
               [else (error "first expression in ifnzero should be a number")]
               )       
            )    
            ]
           [(ifleq? e)
            (let ([e1 (eval-under-env  (ifleq-e1 e) env)]
                  [e2 (eval-under-env (ifleq-e2 e) env)]
                  )
                  (cond [(and (num? e1) (num? e2))
                         (cond
                           [(> (num-int e1) (num-int e2)) (eval-under-env  (ifleq-e4 e) env)]
                           [else (eval-under-env (ifleq-e3 e) env)]
                           )
                         ]
                        [else (error "two first expression in ifleq should be number")]
                    )
                )
            ]
           [(apair? e)
            (let ([v1 (eval-under-env (apair-e1 e) env)]
                  [v2 (eval-under-env (apair-e2 e) env)])
                 (apair v1 v2) 
            )
            ]
             
           [(1st? e)
            (let ([v1 (eval-under-env (1st-e1 e) env)])
              (cond [(apair? v1) (apair-e1 v1)]
                    [else (error "1st arugument should be a apair")])
           )
            ]

          [(2nd? e)
            (let ([v1 (eval-under-env (2nd-e1 e) env)])
              (cond [(apair? v1) (apair-e2 v1)]
                    [else (error "2nd arugument should be a apair")])
           )
            ]
           [(munit? e)
              (munit)]
         [(ismunit? e)
          (let ([v1 (eval-under-env (ismunit-e1 e) env)])
           (cond
             [(munit? v1) (bool #t)]
             [else (bool #f)]))]
         ; since we ourself need to make clousre there is no need ! ?
         ; or maybe it should be checked to avoid situation in which we can not be sure if we have function or not!
         ; to check whether f is valid or not   
         [(closure? e)
           e]
         ; function reduce to a closure.
         [(lam? e)
          (closure env e)
          ]
         ; 
         [(with? e)
          (let ([value1 (eval-under-env (with-e1 e) env)])
             (eval-under-env (with-e2 e) (env_append env (with-s e) value1))
            )
          ]
         ; last 
         [(apply? e)
          (let ([function_closure (eval-under-env (apply-e1 e) env)])
                (cond
                  [(not(closure? function_closure)) (error "apply should be applied to a function")]
                  [else (let ([function (closure-f function_closure)]
                        [function_env (closure-env function_closure)]
                        [function_input (eval-under-env (apply-e2 e) env)])
                    (cond
                      [ (not(lam? function)) (error "function doesn't start with lam!! something seriously is wrong with it!")]
                      [else (let ([function_identifier (lam-s1 function)]
                                  [function_bound_variable_string (lam-s2 function)]
                                  [function_expression (lam-e function)]
                                  )
                             (cond
                              [(not(string? function_bound_variable_string)) (error "function second argument should be a string")]
                              [(null? function_bound_variable_string) (eval-under-env function_expression (env_append function_env function_bound_variable_string function_input))]
                              [(string? function_bound_variable_string) (eval-under-env function_expression (env_append (env_append function_env function_bound_variable_string function_input)
                                                                                    function_identifier (closure function_env function)))]
                              [else (error "something is wrong, probably function bound variable is not a string")]
                              )
                            )
                       ]    
                     )
                    )
                   ]
                 )
             )       
          ]
         


           
           
        ;; CHANGE add more cases here
        [#t (error (format "bad NUMEX expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))

;; Problem 3

(define (ifmunit e1 e2 e3) (cnd (ismunit e1) e2 e3))

(define (with* bs e2)
  (cond [(null? bs) e2]
        [(not(string? (car(car bs)))) (error "list value error value in list of with we have element which is not of type string")] 
        [(with (car(car bs)) (cdr(car bs)) (with* (cdr bs) e2))]
  )
)

(define (ifneq e1 e2 e3 e4)
  (cnd (iseq e1 e2) e4 e3)
)

;; Problem 4

(define numex-filter-fake
  
    (lam "filter" "f"
                           (lam "map" "list"
                                 (cnd (ismunit (var "list")) (munit)   
                                  (with "value" (apply (var "f") (1st (var "list")))
                                       (ifnzero (var "value") (apair (var "value") (apply (var "map") (2nd (var "list")))) (apply (var "map") (2nd (var "list")))
                                        )
                                       )
                                  )
                           )
                                   
    )
  
)
(define numex-filter
  
    (lam "filter" "f"
                           (lam "map" "list"
                                 (cnd (ismunit (var "list")) (munit)   
                                  (with "value" (apply (var "f") (1st (var "list")))
                                       (ifnzero (var "value") (apair (1st (var "list")) (apply (var "map") (2nd (var "list")))) (apply (var "map") (2nd (var "list")))
                                        )
                                       )
                                  )
                           )
                                   
    )
  
)


(define numex-all-gt
  (lam "all-gt" "value"
            (with "f" (lam  ">ToZero" "number" (ifleq (var "number") (var "value") (num 0)(var "number")))  
             (apply numex-filter (var "f") )  
             
             )
  )
)

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function


;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")


;; Do NOT share code with eval-under-env because that will make grading
;; more difficult, so copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")





;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))






;test of numex-all-gt
;(define listOfWonder (apair (num 3)(apair (num 4) (apair (num 5) (munit)))))
;(eval-exp (apply (apply numex-all-gt (num 3)) listOfWonder ))

;test of numex-filter
;(define bloblo (lam "f" "g" (plus (var "g") (num 2))))
;(eval-exp (apply bloblo (num 2)))
;(eval-exp (apply (apply numex-filter bloblo) (munit) ))
;(eval-exp (apply (apply numex-filter bloblo) (apair (num 0) (apair (num -2) (munit)))))








;(eval-exp numex-filter bloblo ) 
;(eval-exp ((numex-filter (lam "k" "g" (plus "g" (num 2)))) (munit)))

;(define numex-all-gt
;  (with "filter" numex-filter
 ;       "CHANGE (notice filter is now in NUMEX scope)"))





;(eval-under-env (with* (list (cons "a" (num 4))) (plus (var "a")(num 3))) null) -> (num 7)

;(eval-under-env (with* (list (cons "a" (num 4)) (cons "b"  (plus (var "a") (num 5)))) (plus (plus (var "b")(num 3)) (var "a"))) null) -> (num 16)

;(eval-exp (ifneq (num 3) (num 3) (num 4) (num 5))) -> (num 5)
;(eval-exp (ifneq (num 3) (num 1) (num 4) (num 5))) -> (num 4)
;(eval-exp (ifneq (bool #t) (bool #t) (num 4)(num 5))) -> (num 5)
;(eval-exp (ifneq (bool #t) (bool #f)(num 4)(num 5))) -> (num 4)





;; 




;;test mimic stack to some point and the mimic a queue for previos tests



;(eval-under-env (ifmunit (munit) (num 3) (bool #t)) null) -> (num 3)
;(eval-under-env (ifmunit (num 4) (num 3) (num 6)) null) -> (num 6)
;(eval-under-env (ifmunit (num 3)) null) -> ifmunit arity mismatch.
;tests for with apply and lam
;(eval-under-env (with "s1" (num 2) (plus(var "s1")(num 7))) null)

;(eval-under-env (apply (lam "g" "k" (plus (var "k") (num 7))) (num 4)) null)

;(eval-under-env (apply (lam "g" "k" (plus (var "k") (num 7))) (num 4)) null)

;(eval-under-env (apply (lam "g" "k" (ifnzero (var "k") (plus (var "k") (apply (var "g")(minus (var "k") (num 1) ))) (num 0))) (num 3)) null)






;(eval-exp (ifleq (num 1) (num 0) (num 3) (num 4)))   -> (num 4)
;(eval-exp (apair (bool #t)(num 2)))    ->    (apair (bool #t) (num 2))  
;(eval-exp (1st(apair (num 2)(num 4))))     -> (num 2)
;(eval-exp(1st(num 4))) -> 1st arugument should be a apair
;(eval-exp(2nd (apair (num 4) (num 3))))  -> (num 3)


;(eval-exp(munit))
;(eval-exp(ismunit(munit)))
;(eval-exp(ismunit(bool #t)))
;(eval-exp(closure null (num 2)))







; tests:

;  test integer
; (eval-exp (num 10.1)) -> error
; (eval-exp (num 10.)) -> return (num 10)

; test boolean
;(eval-exp (bool 123)) -> error
;(eval-exp (bool #t))  -> return (bool #t)
;(eval-exp (bool #f))  -> return (bool #f)

; test add
;(eval-exp (plus (num 10)(num 20)))   -> (num 30)
;(eval-exp (plus (num 10)(bool #t)))  -> NUMEX addition applied to non-number

;test subtraction
;(eval-exp (minus (num 10)(bool #t))) ->  NUMEX subtraction applied to non-number
;(eval-exp (minus (num 12)(num 14)))  -> (num -2)


;test multiplication
;(eval-exp (mult (num 10)(num 4)))    -> (num 40)
;(eval-exp (mult (num 10)(bool #t)))  -> NUMEX multiplication applied to non-number

;test division
;(eval-exp (div (num 10)(num 2)))     -> (num 5)
;(eval-exp (div (num 10)(num 0)))     ->  division by zero in numex
;(eval-exp (div (num 10)(bool #t)))   ->  NUMEX division applied to non-number

;test negation -> ** other test should be carried out **
;(eval-exp (neg (num 10)))   -> (num -10)
;(eval-exp (neg (num -2)))   -> (num 2) 
;(eval-exp (neg (bool #t)))  -> (bool #f)
;(eval-exp (neg (bool #f)))  -> (bool #t)
;(eval-exp (neg (closure 2 3))) -> ?????


;test andalso 
;(eval-exp (andalso (bool #t)(bool #t))) -> (bool #t)
;(eval-exp (andalso (bool #f)(bool #t)))  -> (bool #f)
;(eval-exp (andalso (bool #f)(bool #f)))  -> (bool #f)
;(eval-exp (andalso (bool #t)(bool #f)))  -> (bool #f)
;(eval-exp (andalso (andalso (bool #t)(bool #f))(num 34))) ->  andalso requires two boolean exactly.
;(eval-exp (andalso (andalso (bool #t)(bool #t))(bool #t))) ->  (bool #t)


;test orelse
;(eval-exp (orelse (bool #f)(bool #f)))  -> (bool #f)
;(eval-exp (orelse (bool #t)(bool #f)))  -> (bool #t)
;(eval-exp (orelse (bool #f)(bool #t)))  -> (bool #t)
;(eval-exp (orelse (bool #t)(bool #t)))  -> (bool #t)

; test (cnd e1 e2 e3)
;(eval-exp (cnd (bool #t) (num 1)(num 2)))   -> (num 1)
;(eval-exp (cnd (bool #f) (num 1)(bool #t))) -> (bool #t)
;(eval-exp (cnd (num 4)(num 1)(num 3))) -> in cnd first element should be boolean

; test iseq
;(eval-exp (iseq (bool #t)(bool #f)))  -> (bool #f)
;(eval-exp (iseq (num 1)(num 2)))      -> (bool #f)
;(eval-exp (iseq (num 1)(num 1)))      -> (bool #t)
;(eval-exp (iseq (bool #t)(bool #t)))  -> (bool #t)
;(eval-exp (iseq (bool #t)(num 12)))   -> arguments for iseq should be either bool or num

; test ifnzero
;(eval-exp (ifnzero (num 0) (num 3) (num 4)))     -> (num 4)
;(eval-exp (ifnzero (num 1) (num 3) (num 4)))     -> (num 3)
;(eval-exp (ifnzero (bool #t)(bool #t)(bool #f))) -> first expression should be a number


; test ifleq
; (eval-exp (ifleq (num 1) (num 0) (num 3) (num 4)))
; (eval-exp (ifleq (num 0) (num 1) (num 3) (num 4)))  ->   
; (eval-exp (ifleq (bool #t)(num 1)(num 2)(num 3)))   ->   two first expression in ifleq should be number
;(eval-exp (ifleq (bool #t)(bool #f)(num 3)(num 4))) ->   two first expression in ifleq should be number



