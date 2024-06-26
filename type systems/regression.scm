;;;;;;;;;;;;;;;;;       UNIT TESTING FOR TIMPCORE.SML       ;;;;;;;;;;;;;;;;;

;; step 5 - LITERAL
(check-type 3 int)
(check-type #t bool)
(check-type 'hello sym)

;; step 6 - IFX
(check-type (if #t 9 9) int)
(check-type (if #t #f #t) bool)
(check-type (if #t 'yes 'no) sym)
(check-type-error (if 9 'yes 'no))
(check-type-error (if #t 9 'no))

;; step 7 - VAR
(check-type + (int int -> int))
(check-type > (int int  -> bool))
(check-type-error q)


;; step 9 - VAL and EXP
(val x 5)
(check-type x int)
(check-type 5 int)

;; step 10 - APPLY
(check-type (+ 5 5) int)
(check-type (> 5 5) bool)
(check-type-error (+ #t 5))
(check-type-error (> #t 5))


;; step 11 - LET
(check-type (let ([x 5] [y #t]) (if y x 0)) int)
(check-type-error (let ([x #f] [y #t]) (if y x 0)) )
(check-type-error (let ([x (if #t 6 #f)] [y #t]) (y)))

;; step 12 - LAMBDA
(check-type (lambda ([x : int]) x) (int -> int))
(check-type (lambda ([x : int] [y : int ]) (> y x)) (int int -> bool))
(check-type-error (lambda ([x : list]) x)) 
(check-type-error (lambda ([x : int] [y : bool ]) (> y x))) 

;; step 13 - SET, WHILE, BEGIN
;; SET
(val z #t)
(check-type (set x 7) int)
(check-type (set z #f) bool)
(check-type-error (set x #t))
(check-type-error (set y 7))

;;WHILE
(check-type (while #t 7) unit)
(check-type (while #f x) unit)
(check-type (while (> 0 1) 7) unit)
(check-type-error (while 7 7))
(check-type-error (while #t (> 9 #t)))

;;BEGIN
(check-type (begin ) unit)
(check-type (begin (while #t 7) (+ 5 5)) int)
(check-type-error (begin (while #t 7) (> 5 #t)))

;; step 14 - LETSTAR
(check-type (let* ([x 5] [y x] [z 10]) (+ (+ x y) z)) int)
(check-type (let* [] (+ 1 1))  int)
(check-type-error (let* ([x 5] [y x] [z #t]) (+ (+ x y) z)))

;; step 15 - LETREC
(check-type (letrec [([x : (int -> int)] (lambda ([a : int]) (y 1))) 
                ([y : (int -> int)] (lambda ([b : int]) b))] 
                (+ (x 1) (y 2))) int)
(check-type-error (letrec [([x : int] (lambda ([a : int]) (y 1))) 
                ([y : (int -> int)] (lambda ([b : int]) b))] (+ (x 1) (y 2))))
(check-type-error (letrec [([x : (int -> int)] (lambda ([a : int]) (y 1))) 
                        ([y : (int -> int)] (lambda ([b : int]) b))] 
                        (+ (x #t) (y #f))))

;; step 16 - VALREC
(val-rec [recfunc : (int -> int)] (lambda ([n : int]) (recfunc (- n 1))))
(check-type recfunc (int -> int))
(check-type-error (val-rec [func : (int -> int)] (lambda ([n : int]) (#f))))

(define int sum ([x : int] [y : int]) (+ x y))
(check-type sum (int int -> int))
(check-type-error (define int func1 ([x : int] [y : int]) (> 0 1)))
(check-type-error (define bool func2 ([x : bool] [y : bool]) (> x y)))


;; step 17 - TYLAMBDA, TYAPPLY 
;; TYLAMBDA 
(check-type (type-lambda ['a] (lambda ([x : 'a]) x)) (forall ['a] ('a -> 'a)))
(check-type (type-lambda ['a] (lambda ([x : 'a] [y : 'a]) ((@ = 'a) x y))) 
                                                (forall ['a ] ('a 'a -> bool)))
(check-type-error (type-lambda ['a] (lambda ([y : 'a]) ((@ = 'a) x y))))


;; TYAPPLY
(check-type (@ (type-lambda ['a] (lambda ([x : 'a]) x)) int) (int -> int))
(check-type (@ (type-lambda ['a] (lambda ([x : 'a] [y : 'a]) 
                                 ((@ = 'a) x y))) int) (int int -> bool))
(check-type ((@ = int) 7 8) bool)
(check-type-error (@ + sym))

;; step 18 - LITERAL LISTS
;; empty list
(check-type (@ '() int) (list int))
(check-type (@ '() bool) (list bool))
(check-type-error (@ '() list))

;;singelton list
(check-type '(5) (list int))
(check-type '((4)) (list (list int)))
(check-type '(((#t))) (list (list (list bool))))
(check-type-error '(> 0 1))

;; non-empty list 
(check-type '(5 4 3) (list int))
(check-type '((5) (4) (3)) (list (list int)))
(check-type '(#t #f #f #t) (list bool))
(check-type-error '(a b 4 5))
(check-type-error '(#t (> 0 1)))